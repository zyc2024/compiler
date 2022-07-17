open Cli
open FileManager
open ArgParser
open Lex

exception ArgumentError of string

let specs =
  let open ArgParser in
  let sourcepath_checker dir =
    if FileManager.directory_exists dir then ()
    else
      raise
        (ArgumentError
           (Printf.sprintf
              "provided sourcepath directory \"%s\" does not exists" dir))
  in
  let destination_checker dir =
    if FileManager.file_exists dir then
      raise
        (ArgumentError
           (Printf.sprintf
              "provided output directory \"%s\" currently does not exists and \
               cannot be created because a file named \"%s\" exists"
              dir dir))
    else ()
  in
  [
    make_spec_no_arg "--help" ~alternatives:[ "-help"; "-h" ]
      "displays this help message and exits";
    make_spec_no_arg "--lex" ~alternatives:[ "-l" ]
      "Generate output from lexical analysis";
    make_spec_no_arg "--debug" "enable debugging mode for developers";
    make_spec "--sourcepath" ~alternatives:[ "-sp"; "-source" ]
      ~description:"Specify where to look for input files" ~arg:"path"
      sourcepath_checker;
    make_spec "-d"
      ~description:
        "Specify where to place generated assembly/executable output files"
      ~arg:"dir" destination_checker;
    make_spec "-D"
      ~description:"Specify where to place generated diagnostic files"
      ~arg:"dir" destination_checker;
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

type error_type =
  | LEXICAL
  | SYNTAX
  | TYPE

(** [compile_time_exit error_type file_name line col msg] prints the error
    message [msg] to stdout and halts the compiler. Unlike [error_exit msg], the
    printed message has format
    [<name> error beginning at <file_name>:%d:%d error:<msg>]*)
let compile_time_exit error_type file_name line col msg =
  let error_name =
    match error_type with
    | LEXICAL -> "Lexical"
    | SYNTAX -> "Syntax"
    | TYPE -> "Type"
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
  try ArgParser.parse cmd_args specs with
  | MissingPositionalArgument flag ->
      error_exit_detailed ("missing positional argument for " ^ flag)
  | UnsupportedFlag flag ->
      error_exit_detailed ("compiler does not support the flag " ^ flag)
  | ArgumentError error -> error_exit error

(*** [files] is the list of input files to the compiler. *)
let files =
  try ArgParser.files parsed_cmd_args [ "evo" ]
  with IncorrectFileExtension file ->
    error_exit ("unrecognized file name format: " ^ file)

(* check that there are input files and halt the compiler if input files are not
   provided*)
let _ =
  match files with [] -> error_exit_detailed "missing input files" | _ -> ()

(** compiler_state represents the current state of the compiler including
    enabled optimizations, source paths, output paths, etc. *)
type compiler_state = {
  sourcepath : string;
  diagnosticpath : string;
  output_lex : bool;
  debug : bool; (* add more as needed *)
}

(** [initial_state] is the starting compiler state when envoke is called upon. *)
let initial_state : compiler_state =
  { sourcepath = "."; diagnosticpath = "."; output_lex = false; debug = false }

(** [update_state lst state] is an updated compiler state when specific flags
    trigger any updates in file paths, desired actions, or optimizations. Per
    the command line specification, the compiler will halt when the help flag is
    enabled. *)
let rec update_state flag_arg_lst state =
  match flag_arg_lst with
  | [] -> state
  | (flag, value) :: t -> (
      match flag with
      | "--help" ->
          print_usage ();
          exit 0
      | "--lex" -> update_state t { state with output_lex = true }
      | "--sourcepath" -> update_state t { state with sourcepath = value }
      | "--debug" -> update_state t { state with debug = true }
      | "-D" -> update_state t { state with diagnosticpath = value }
      | _ -> (* for type system exhaustiveness, should not match *) state)

(** [current_state] is the new starting compiler state after enabling all the
    requested (valid) flags and recording their corresponding argument values
    (if any). *)
let current_state =
  update_state (ArgParser.flag_and_args parsed_cmd_args) initial_state

(** [lex source_name in_chan] lexes the content from the input_channel. When
    command flag [--lex] is enabled, compiler makes an attempt to output token
    list to file at location [./diagnosticpath/source_name]. If this is not
    possible because a writer cannot be created, the compiler is halted. *)
let lex source_name in_chan =
  let open LexUtil in
  let base_name = Filename.basename source_name in
  if current_state.output_lex then (
    let output_file =
      Filename.concat current_state.diagnosticpath
        (Filename.chop_suffix source_name ".evo" ^ ".lexed")
    in
    let _ = FileManager.mkdir_if_nonexistent current_state.diagnosticpath in
    let out_chan =
      try FileManager.open_writer output_file
      with NameTakenByDirectory name ->
        Printf.sprintf
          "the file %s cannot be created possibly because a directory exists \
           with the same name."
          name
        |> error_exit
    in
    (try lex_with_output in_chan out_chan
     with Lexical_error (l, c, msg) ->
       (* force lexing to finish and halt the compiler with error message *)
       FileManager.close_writer out_chan;
       compile_time_exit LEXICAL base_name l c msg);
    (* lexing finished *)
    FileManager.close_writer out_chan)
  else
    try lex_no_output in_chan
    with Lexical_error (l, c, msg) ->
      compile_time_exit LEXICAL base_name l c msg

(** [compilation_run source_name] compiles the given file and if source file
    does not exists in sourcepath, the compiler halts and reports the error.*)
let compilation_run source_name =
  (* the file name is relative to the sourcepath with path prepended to the
     source name. *)
  let file_name = Printf.sprintf "%s/%s" current_state.sourcepath source_name in
  try
    let in_chan = FileManager.open_reader file_name in
    lex source_name in_chan
  with NameTakenByDirectory _ | FileNotExists _ ->
    error_exit ("the given file cannot be found: " ^ file_name)

let compile () = List.iter compilation_run files
let _ = compile ()
