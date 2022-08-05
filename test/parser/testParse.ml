(* .evo is source file *)
(* .evo.expected is an s-expr file *)
(* .parsed is the produced file *)

(* stolen from the testLex *)

(** [soucr_directories] is the list of absolute path directories under the
    test/lexer directory. All test cases must be placed in labeled directories
    (this is more organized than having 20+ test files and expected outputs in
    one directory.)*)
let source_directories =
  Sys.chdir "./test/parser";
  let files = Sys.readdir "." in
  let cwd = Sys.getcwd () in
  List.filter (fun file -> Sys.is_directory file) (Array.to_list files)
  |> List.map (fun dir -> Printf.sprintf "%s/%s" cwd dir)

(** [get_source_files dir] is the list of evo source files in the given absolute
    path directory [dir] *)
let get_source_files dir =
  let files = Sys.readdir dir in
  List.filter (fun x -> Filename.extension x = ".evo") (Array.to_list files)
  |> List.map (fun file -> Printf.sprintf "%s/%s" dir file)

let source_files =
  let rec add_files files = function
    | [] -> files
    | dir :: t ->
        let dir_files = get_source_files dir in
        add_files (List.append dir_files files) t
  in
  add_files [] source_directories

(* let print_list list = List.iter (fun str -> print_endline str) list let _ =
   print_list source_files *)
let total = List.length source_files
let main_parser = Parse.parse_module
let main_lexer = Lex.Lexer.tokenize
let main_ast_transformer = Ast.SexpConvert.sexp_of_file

let parse_file fname =
  try
    let istream = open_in fname in
    let slbuf = Sedlexing.Utf8.from_channel istream in
    let lexert = Lex.Lexer.make_lexer slbuf in
    let token_ref = ref Parse.EOF in
    let tokenizer _ =
      let t = main_lexer lexert in
      token_ref := t;
      t
    in
    let generic_lexer = Sedlexing.with_tokenizer tokenizer slbuf in
    let parser = MenhirLib.Convert.Simplified.traditional2revised main_parser in
    parser generic_lexer
  with Parse.Error ->
    failwith (Printf.sprintf "Failed to parse \"%s\"\n" fname)

let rec run_parse_tests nopassed = function
  | fname :: t -> (
      let basename = Filename.chop_suffix fname ".evo" in
      let parsed_file = parse_file fname in
      let ostream = open_out (basename ^ ".parsed") in
      let oformat = Format.formatter_of_out_channel ostream in
      Ast.SexpConvert.print_sexp oformat (main_ast_transformer parsed_file);
      Format.pp_print_flush oformat ();
      close_out ostream;
      (* read in the file jsut generated and .expected *)
      try
        let gend = open_in (basename ^ ".parsed") in
        let expect = open_in (fname ^ ".expected") in
        let res =
          Eth.tokenized_compare fname ParsedGrammar.parseParsed ( = )
            ParsedGrammar.End ParsedGrammar.str_of_tok expect gend
        in
        match res with
        | Ok () ->
            print_endline (basename ^ ": ok");
            run_parse_tests (nopassed + 1) t
        | Error s ->
            print_endline (Printf.sprintf "%s: %s" basename s);
            run_parse_tests nopassed t
      with Sys_error _ ->
        Printf.eprintf "Failed to locate files for %s\n" basename;
        run_parse_tests nopassed t)
  | [] ->
      print_endline
        (Printf.sprintf "%d out of %d test cases passed\n" nopassed total)

let _ = run_parse_tests 0 source_files
