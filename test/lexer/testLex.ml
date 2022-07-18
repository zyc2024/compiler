open TestUtil
open Lex.LexUtil

let run_lex fname =
  let f = open_in fname in
  let o = open_out (Filename.chop_suffix fname ".evo" ^ ".lexed") in
  (try lex_with_output f o with Lexical_error _ -> ());
  close_out o

let source_files =
  Sys.chdir "./test/lexer";
  let files = Sys.readdir "." in
  List.filter (fun x -> Filename.extension x = ".evo") (Array.to_list files)

let total = List.length source_files

let rec test_files passed = function
  | file_name :: t -> begin
      run_lex file_name;
      let lexed_file_name = Filename.chop_suffix file_name ".evo" ^ ".lexed" in
      let expected = file_name ^ ".expected" |> open_in in
      let lexed = lexed_file_name |> open_in in
      match Eth.compare lexed_file_name expected lexed with
      | Ok _ ->
          print_endline (file_name ^ ": ok");
          test_files (passed + 1) t
      | Error s ->
          print_endline (Printf.sprintf "%s: %s" file_name s);
          test_files passed t
    end
  | [] ->
      print_endline
        (Printf.sprintf "%d out of %d test cases passed\n" passed total)

let () = test_files 0 source_files
