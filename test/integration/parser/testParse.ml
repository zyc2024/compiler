let run_parse fname =
  let dir = Filename.dirname fname in
  let base = Filename.basename fname in
  let compiler_command =
    Printf.sprintf "evoke --parse --sourcepath %s -D %s -- %s" dir dir base
  in
  let _ = Sys.command compiler_command in
  ()

(** [soucr_directories] is the list of absolute path directories under the
    test/lexer directory. All test cases must be placed in labeled directories
    (this is more organized than having 20+ test files and expected outputs in
    one directory.)*)
let source_directories =
  Sys.chdir "./test/integration/parser";
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
      run_parse file_name;
      let parsed_file_name = Filename.remove_extension file_name ^ ".parsed" in
      let expected_file_name = parsed_file_name ^ ".expected" in
      let test_name = Filename.basename file_name in
      try
        let expected = open_in expected_file_name in
        begin
          try
            let input = open_in parsed_file_name in
            match
              Eth.compare_sexp
                (Filename.basename parsed_file_name)
                ~expected ~input
            with
            | Ok _ ->
                print_endline (test_name ^ ": ok");
                test_files (passed + 1) t
            | Error s ->
                ANSITerminal.(
                  print_string [ Foreground Red ]
                    (Printf.sprintf "%s: %s\n" test_name s));
                test_files passed t
          with Sys_error _ ->
            ANSITerminal.(
              print_string [ Foreground Yellow ]
                (Printf.sprintf "%s: file %s cannot be found\n" test_name
                   parsed_file_name));
            test_files passed t
        end
      with Sys_error _ ->
        ANSITerminal.(
          print_string
            [ Foreground Yellow; Blink ]
            (Printf.sprintf "%s: file %s cannot be found\n" test_name
               expected_file_name));
        test_files passed t
    end
  | [] ->
      print_endline
        (Printf.sprintf "%d out of %d test cases passed" passed total)

let () = test_files 0 source_files
