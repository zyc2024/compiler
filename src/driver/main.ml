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
    ]

let print_usage () =
  print_endline
    "Usage: envoke <flag options> <source files>\n\
     where possible options include:";
  let printed_specs = List.sort ArgParser.compare_name specs in
  List.iter
    (fun spec ->
      Printf.fprintf stdout "  %s\n" (ArgParser.spec_usage spec);
      Printf.fprintf stdout "\t%s\n" (ArgParser.description spec))
    printed_specs

(** [error_exit_detailed msg] prints the error message [msg] to the terminal as
    well as the usage specification before halting the compiler.*)
let error_exit_detailed msg =
  print_endline ("error: " ^ msg);
  print_usage ();
  exit 0

(** [error_exit msg] prints to stdout the error message [msg] and halts the
    compiler. *)
let error_exit msg =
  print_endline ("error: " ^ msg);
  exit 0

(** [compile_time_exit error_type file_name line col msg] prints the error
    message [msg] to stdout and halts the compiler. Unlike [error_exit msg], the
    printed message has format
    [<name> error beginning at <file_name>:%d:%d error:<msg>]*)
let compile_time_exit error_type file_name line col msg =
  let error_name =
    match error_type with
    | `LEXICAL -> "Lexical"
    | `SYNTAX -> "Syntax"
    | `SEMANTIC -> "Semantic"
    | `TYPE -> "Type"
  in
  print_endline
    (Printf.sprintf "%s error beginning at %s:%d:%d error:%s" error_name
       file_name line col msg);
  exit 0

let cmd_args = Array.to_list Sys.argv

(* halt the compiler when no flags/inputs are passed. this defaults to passing
   in the help flag. The minimum number of arguments is 1. *)
let _ =
  if List.length cmd_args = 1 then (
    print_usage ();
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

(* check that there are input files and halt the compiler if input files are not
   provided*)
let _ =
  match source_files with
  | [] -> error_exit_detailed "missing input files"
  | _ -> ()

(** compiler_state represents the modifable state of the compiler including
    enabled optimizations, source paths, output paths, etc. *)
type compiler_state = {
  mutable sourcepath : string;
  mutable diagnostic_path : string;
  mutable output_lex : bool;
  mutable debug : bool; (* add more as needed *)
  mutable output_parse : bool;
  mutable output_path : string;
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
        (Printf.sprintf "provided sourcepath directory \"%s\" does not exists"
           dir)
  in
  let check_destination dir =
    if FileManager.file_exists dir then
      error_exit
        (Printf.sprintf
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
          print_usage ();
          exit 0
      | "--lex" -> state.output_lex <- true
      | "--sourcepath" ->
          check_sourcepath value;
          state.sourcepath <- value
      | "--debug" -> state.debug <- true
      | "-D" ->
          check_destination value;
          state.diagnostic_path <- get_full_path value
      | "-d" ->
          check_destination value;
          state.output_path <- get_full_path value
      | "--parse" -> state.output_parse <- true
      | _ -> (* for type system exhaustiveness, should not match *) ());
      update_state t state

(** [current_state] is the new starting compiler state after enabling all the
    requested (valid) flags and recording their corresponding argument values
    (if any). *)
let current_state =
  let initial_state =
    {
      sourcepath = ".";
      diagnostic_path = ".";
      output_lex = false;
      debug = false;
      output_parse = false;
      output_path = ".";
    }
  in
  update_state (ArgParser.get_flag_and_args parsed_cmd_args) initial_state;
  initial_state

let report_create_failed name =
  Printf.sprintf
    "the file %s cannot be created possibly because a directory exists with \
     the same name."
    name
  |> error_exit

(** [lex source_name in_chan] lexes the content from the input_channel. When
    command flag [--lex] is enabled, compiler makes an attempt to output token
    list to file at location [./diagnosticpath/source_name]. If this is not
    possible because a writer cannot be created, the compiler is halted. *)
let lex source_name in_chan =
  let open Lex in
  let base_name = Filename.basename source_name in
  let lex_aux ~run ~clean_up =
    match run () with
    | () -> clean_up () (* on success, may have to close some streams*)
    | exception Lexical_error (pos, msg) ->
        clean_up ();
        let l, c = Util.Position.coord_of_pos pos in
        compile_time_exit `LEXICAL base_name l c msg
  in
  if current_state.output_lex then
    let output_file_name =
      Filename.concat current_state.diagnostic_path
        (Filename.chop_suffix source_name ".evo" ^ ".lexed")
    in
    let _ = FileManager.mkdir_if_nonexistent current_state.diagnostic_path in
    let out_chan =
      try FileManager.open_writer output_file_name
      with FileManager.NameTakenByDirectory name -> report_create_failed name
    in
    lex_aux
      ~run:(fun () -> lex_with_output in_chan out_chan)
      ~clean_up:(fun () ->
        FileManager.close_writer out_chan;
        FileManager.close_reader in_chan)
  else
    lex_aux
      ~run:(fun () -> lex_no_output in_chan)
      ~clean_up:(fun () -> FileManager.close_reader in_chan)

let parse source_name in_chan =
  let open Lex in
  let sedlexbuf = Sedlexing.Utf8.from_channel in_chan in
  let tokenizer _ = Lexer.(tokenize (make_lexer sedlexbuf)) in
  let token_generator = Sedlexing.with_tokenizer tokenizer sedlexbuf in
  if current_state.output_parse then begin
    let output_file =
      Filename.concat current_state.diagnostic_path
        (Filename.chop_suffix source_name ".evo" ^ ".parsed")
    in
    let _ = FileManager.mkdir_if_nonexistent current_state.diagnostic_path in
    let out_chan =
      try FileManager.open_writer output_file
      with FileManager.NameTakenByDirectory name -> report_create_failed name
    in
    let fmt = Format.formatter_of_out_channel out_chan in
    let ast =
      match Parse.parse_with_output token_generator `Module fmt with
      | Ok ast -> ast
      | Error (pos, msg) ->
          let l, c = Util.Position.coord_of_pos pos in
          Format.pp_print_flush fmt ();
          FileManager.close_writer out_chan;
          FileManager.close_reader in_chan;
          compile_time_exit `SYNTAX (Filename.basename source_name) l c msg
    in
    Format.pp_print_flush fmt ();
    FileManager.close_writer out_chan;
    FileManager.close_reader in_chan;
    let _ = ast in
    ()
  end
  else
    let ast =
      match Parse.parse token_generator `Module with
      | Ok ast -> ast
      | Error (pos, msg) ->
          let l, c = Util.Position.coord_of_pos pos in
          FileManager.close_reader in_chan;
          compile_time_exit `SYNTAX (Filename.basename source_name) l c msg
    in
    FileManager.close_reader in_chan;
    (* for compilation to quiet *)
    let _ = ast in
    ()

(** [compilation_run source_name] compiles the given file and if source file
    does not exists in sourcepath, the compiler halts and reports the error.*)
let compilation_run source_name =
  (* the file name is relative to the sourcepath with path prepended to the
     source name. *)
  let file_name = Printf.sprintf "%s/%s" current_state.sourcepath source_name in
  let in_chan =
    try FileManager.open_reader file_name
    with FileManager.NameTakenByDirectory _ | FileManager.FileNotExists _ ->
      error_exit ("the given file cannot be found: " ^ Unix.realpath file_name)
  in
  lex source_name in_chan;
  parse source_name (FileManager.open_reader file_name)

let compile () = List.iter compilation_run source_files
let _ = compile ()
