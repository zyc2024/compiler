(* run_nth_test *)
let run_lex fname =
  let f = open_in fname in
  let o = open_out (fname ^ ".lexed") in

  Lex.LexUtil.lex_with_output f o;
  close_out o

let source_files =
  let files = Sys.readdir "." in
  List.filter (fun x -> Filename.extension x = ".evo") (Array.to_list files)

let rec test_files = function
  | h :: t ->
      run_lex h;
      test_files t
  | [] -> print_endline "Done running"

let () = test_files [ "A.evo" ]
