open Lex.LexUtil

let run_lex fname =
  let f = open_in fname in
  let o = open_out (Filename.chop_suffix fname ".evo" ^ ".lexed") in
  (try lex_with_output f o with Lexical_error _ -> ());
  close_out o

(** [soucr_directories] is the list of absolute path directories under the
    test/lexer directory. All test cases must be placed in labeled directories
    (this is more organized than having 20+ test files and expected outputs in
    one directory.)*)
let source_directories =
  Sys.chdir "./test/lexer";
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

let rec test_files passed = function
  | file_name :: t -> begin
      run_lex file_name;
      let lexed_file_name = Filename.chop_suffix file_name ".evo" ^ ".lexed" in
      let test_name = Filename.basename file_name in
      try
        let expected = file_name ^ ".expected" |> open_in in
        let input = lexed_file_name |> open_in in
        match
          Eth.compare (Filename.basename lexed_file_name) ~expected ~input
        with
        | Ok _ ->
            print_endline (test_name ^ ": ok");
            test_files (passed + 1) t
        | Error s ->
            print_endline (Printf.sprintf "%s: %s" test_name s);
            test_files passed t
      with Sys_error s ->
        print_endline (test_name ^ ": " ^ s);
        test_files passed t
    end
  | [] ->
      print_endline
        (Printf.sprintf "%d out of %d test cases passed" passed total)

let () = test_files 0 source_files