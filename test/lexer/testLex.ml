


let rec pStrList = function
    | h::t -> print_endline h ; pStrList t
    | [] -> ()



(* run_nth_test  *)
let run_named_test fname = 
    let f = open_in fname in 
    let o = open_out (fname ^ ".output") in 
    Lex.LexUtil.output_tokens f o;;



let () = run_named_test "A.evo"


let get_files = 
    let tfiles = Sys.readdir "." in 
    List.filter (fun x -> Filename.extension x = ".evo") (Array.to_list tfiles) ;;

let rec test_files = function   
    | h::t ->  run_named_test h ; test_files t
    | [] -> print_endline "Done running" ;;

(* let () = let tfiles = Array.to_list (Sys.readdir ".") in 

    in pStrList tfiles *)



let () = test_files get_files ;;