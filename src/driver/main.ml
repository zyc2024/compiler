open Cli

let specs =
  ArgParser.
    [
      make_spec "--help" ~alternatives:[ "-help"; "-h" ]
        "displays this help message and exits";
      make_spec "--lex" ~alternatives:[ "-l" ]
        "Generate output from lexical analysis";
      make_spec "--debug" "enable debugging mode for developers";
      make_spec "--sourcepath" ~alternatives:[ "-sp"; "-source" ] ~arg:"path"
        "Specify where to look for input files";
      make_spec "-d" ~arg:"dir"
        "Specify where to place generated assembly/executable output files";
      make_spec "-D" ~arg:"dir"
        "Specify where to place generated diagnostic files";
      make_spec "--parse" ~alternatives:[ "-p" ]
        "Generate output from syntatic analysis";
      make_spec "-verbose" "Output messages about what the compiler is doing";
    ]

let str_format = Printf.sprintf

let print_usage oc () =
  Printf.fprintf oc
    "Usage: evoke <flag options> <source files>\n\
     where possible options include:";
  let printed_specs = List.sort ArgParser.compare_name specs in
  List.iter
    (fun spec ->
      Printf.fprintf oc "  %s\n" (ArgParser.spec_usage spec);
      Printf.fprintf oc "\t%s\n" (ArgParser.description spec))
    printed_specs

(** [error_exit_detailed msg] prints the error message [msg] to the terminal as
    well as the usage specification before halting the compiler.*)
let error_exit_detailed msg =
  Printf.eprintf "error: %s\n" msg;
  print_usage stderr ();
  exit 2

(** [error_exit msg] prints to stdout the error message [msg] and halts the
    compiler. *)
let error_exit msg =
  Printf.eprintf "error: %s\n" msg;
  exit 2

(** [compile_time_exit error_type source_name source_loc line col msg] prints
    the error message [msg] to stdout and halts the compiler. Unlike
    [error_exit msg], the printed message has format
    [<name> error beginning at <source_name>:%d:%d error:<msg>] and prints the
    line of error from file [source_loc] as well as placing a caret pointing at
    the given error column.*)
let compile_time_exit error_type source_name source_loc line col msg =
  let ic = FileManager.open_reader source_loc in
  let nth_line n =
    let rec iter line = function
      | 0 -> line
      | x -> iter (try input_line ic with End_of_file -> "") (x - 1)
    in
    iter "" n
  in
  let error_name =
    match error_type with
    | `LEXICAL -> "Lexical"
    | `SYNTAX -> "Syntax"
    | `SEMANTIC -> "Semantic"
    | `TYPE -> "Type"
  in
  ANSITerminal.prerr_string [ Bold ]
    (str_format "%s error beginning at %s:%d:%d" error_name source_name line col);
  ANSITerminal.(prerr_string [ red; Bold ] " error: ");
  ANSITerminal.prerr_string [ Bold ] (msg ^ "\n");
  ANSITerminal.prerr_string [] (str_format "%s\n" (nth_line line));
  Printf.eprintf "%*s" (col - 1) "";
  ANSITerminal.(prerr_string [ green; Bold ] "^\n");
  FileManager.close_reader ic;
  exit 2

let cmd_args = Array.to_list Sys.argv

(* halt the compiler when no flags/inputs are passed. this defaults to passing
   in the help flag. The minimum number of arguments is 1. *)
let _ =
  if List.length cmd_args = 1 then (
    print_usage stdout ();
    exit 0)
  else ()

(** [parsed_cmd_args] is the result data type from reading the CLI arguments to
    envoke. *)
let parsed_cmd_args =
  match ArgParser.parse cmd_args specs with
  | Ok result -> result
  | Error s -> error_exit s

(** [source_files] is the list of input files provided to the compiler. *)
let source_files =
  match ArgParser.get_files parsed_cmd_args [ "evo" ] with
  | Ok files -> files
  | Error file_name -> error_exit ("unrecognized file name format: " ^ file_name)

(** compiler_state represents the modifable state of the compiler including
    enabled optimizations, source paths, output paths, etc. *)
type compiler_state = {
  mutable sourcepath : string;
  mutable diagnostic_path : string;
  mutable output_lex : bool;
  mutable debug : bool;
  mutable output_parse : bool;
  mutable output_path : string;
  mutable verbose : bool;
}

(** [update_state lst state] updates [state] when specific flags trigger any
    updates in file paths, desired actions, or optimizations. Per the command
    line specification, the compiler will halt gracefully when the help flag is
    enabled. *)
let rec update_state flag_arg_lst state =
  let check_sourcepath dir =
    if FileManager.directory_exists dir then ()
    else
      error_exit
        (str_format "provided sourcepath directory \"%s\" does not exists" dir)
  in
  let check_destination dir =
    if FileManager.file_exists dir then
      error_exit
        (str_format
           "provided output directory \"%s\" currently does not exists and \
            cannot be created because a file named \"%s\" exists"
           dir dir)
    else ()
  in
  (* convert a path to its absolute form if it is relative. Otherwise, the path
     is already in absolute form*)
  let get_full_path dir =
    if Filename.is_relative dir then Format.sprintf "%s/%s" (Sys.getcwd ()) dir
    else dir
  in
  match flag_arg_lst with
  | [] -> ()
  | (flag, value) :: t ->
      (match flag with
      | "--help" ->
          print_usage stdout ();
          exit 0
      | "--lex" -> state.output_lex <- true
      | "--sourcepath" ->
          let path = get_full_path value in
          check_sourcepath path;
          state.sourcepath <- Unix.realpath path
      | "--debug" -> state.debug <- true
      | "-D" ->
          let path = get_full_path value in
          check_destination path;
          state.diagnostic_path <- path
      | "-d" ->
          let path = get_full_path value in
          check_destination path;
          state.output_path <- path
      | "--parse" -> state.output_parse <- true
      | "-verbose" -> state.verbose <- true
      | _ -> (* for type system exhaustiveness, should not match *) ());
      update_state t state

(** [current_state] is the new starting compiler state after enabling all the
    requested (valid) flags and recording their corresponding argument values
    (if any). *)
let current_state =
  let initial_state =
    {
      sourcepath = FileManager.cwd;
      diagnostic_path = FileManager.cwd;
      output_lex = false;
      debug = false;
      output_parse = false;
      output_path = FileManager.cwd;
      verbose = false;
    }
  in
  update_state (ArgParser.get_flag_and_args parsed_cmd_args) initial_state;
  initial_state

(* check that there are input files and halt the compiler if input files are not
   provided*)
let _input_check =
  match source_files with
  | [] -> error_exit_detailed "missing input files"
  | _ -> ()

(** [report_create_failed name] halts the compiler with a message indicating
    that a file cannot be created under [name]*)
let report_create_failed name =
  str_format
    "the file %s cannot be created possibly because a directory exists with \
     the same name."
    name
  |> error_exit

let print_time_acc event start_time f x =
  let t1 = Sys.time () in
  let fx = f x in
  if current_state.verbose then
    Printf.printf "[%s completed in %d ms]\n" event
      (start_time +. (1000. *. (Sys.time () -. t1))
      |> Float.round |> Float.to_int)
  else ();
  fx

(** [print_time event f x] displays to standard output the time in ms elapsed on
    computing [f x] when verbose mode is on, otherwise this only returns the
    value of [f x]. *)
let print_time event f x = print_time_acc event Float.zero f x

let elapse_time f x =
  let t1 = Sys.time () in
  let fx = f x in
  let elapsed = 1000. *. (Sys.time () -. t1) in
  (fx, elapsed)

(** [src_abspath source] is the absolute path of a given source input name.

    Example: if the compiler received [/some/dir] as sourcepath and
    [path/to/file] as the source name, then the source file path is
    [/some/dir/path/to/file]. *)
let src_abspath source = str_format "%s/%s" current_state.sourcepath source

(** [diagnostic_name source] is the absolute path of the diagnostic output files
    without an extension. An extension can be concatenated to
    [diagnostic_name source] to produce an absolute diagnostic output file path.*)
let diagnostic_name source =
  str_format "%s/%s" current_state.diagnostic_path
    (Filename.remove_extension source)

let lex source_name ic =
  let lexer = Lexer.make_lexer (Sedlexing.Utf8.from_channel ic) in
  let lex_aux ~handle ~result_handler =
    let result, lex_time = elapse_time (Lexer.lex ~handle) lexer in
    match result with
    | Ok handle_time -> result_handler lex_time handle_time
    | Error (handle_time, position, msg) ->
        result_handler lex_time handle_time;
        let l, c = Util.Position.coord_of_pos position in
        compile_time_exit `LEXICAL source_name (src_abspath source_name) l c msg
  in
  if current_state.output_lex then
    let oc =
      try FileManager.open_writer (diagnostic_name source_name ^ ".lexed")
      with FileManager.NameTakenByDirectory name -> report_create_failed name
    in
    let fmt = Format.formatter_of_out_channel oc in
    let lexed = Filename.remove_extension source_name ^ ".lexed" in
    let result_handler lex_time print_time =
      print_time_acc "lexing" (lex_time -. print_time) (fun () -> ()) ();
      print_time_acc ("writing to " ^ lexed) print_time
        (Format.pp_print_flush fmt)
        ();
      FileManager.close_writer oc;
      FileManager.close_reader ic
    in
    lex_aux ~handle:(`Print fmt) ~result_handler
  else
    lex_aux ~handle:`No_print ~result_handler:(fun time _ ->
        print_time_acc "lexing" time (fun () -> ()) ();
        FileManager.close_reader ic)

let parse source_name ic =
  let lexbuf = Sedlexing.Utf8.from_channel ic in
  let token_generator = Lexer.make_lexer lexbuf |> Lexer.make_token_generator in
  let parse_aux ~ast_handler ~error_handler =
    match print_time "parsing" (Parse.parse token_generator) `Module with
    | Ok ast ->
        ast_handler ast;
        ast
    | Error (pos, msg) ->
        let l, c = Util.Position.coord_of_pos pos in
        error_handler (l, c, msg);
        compile_time_exit `SYNTAX source_name (src_abspath source_name) l c msg
  in
  if current_state.output_parse then
    let output_file = diagnostic_name source_name ^ ".parsed" in
    (* let _ = FileManager.mkdir_if_nonexistent current_state.diagnostic_path
       in *)
    let oc =
      try FileManager.open_writer output_file
      with FileManager.NameTakenByDirectory name -> report_create_failed name
    in
    let fmt = Format.formatter_of_out_channel oc in
    let printer = Util.SexpPrinter.make_printer fmt in
    let clean_up () =
      Format.pp_print_flush fmt ();
      FileManager.close_writer oc
    in
    parse_aux
      ~ast_handler:(fun ast ->
        print_time "printing ast"
          (fun ast -> Ast.SexpConvert.(print_sexp printer (sexp_of_file ast)))
          ast;
        clean_up ())
      ~error_handler:(fun (l, c, msg) ->
        Format.fprintf fmt "%d:%d error:%s\n" l c msg;
        clean_up ())
  else parse_aux ~ast_handler:(fun _ast -> ()) ~error_handler:(fun _ -> ())

(** [compilation_run source_name] compiles the given file and if source file
    does not exists in sourcepath, the compiler halts and reports the error.*)
let compilation_run source_name =
  let file_loc = src_abspath source_name in
  let in_chan =
    try FileManager.open_reader file_loc
    with FileManager.NameTakenByDirectory _ | FileManager.FileNotExists _ ->
      error_exit (str_format "cannot find input file %s" source_name)
  in
  print_time "lexing task" (lex source_name) in_chan;
  let _ast =
    print_time "parsing task" (parse source_name)
      (FileManager.open_reader file_loc)
  in
  ()

let compile () =
  List.iter
    (fun source ->
      print_time ("compilation of " ^ source) compilation_run source)
    source_files

let _run = compile ()
