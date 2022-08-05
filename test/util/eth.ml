let input_line_opt ic = try Some (input_line ic) with End_of_file -> None

let error_msg line file_name s1 s2 =
  Printf.sprintf
    "Mismatch detected at line %d of file %s\n\texpected: %s\n\tfound: %s" line
    file_name s1 s2

let compare file_name ic1 ic2 =
  let rec loop line =
    let s1, s2 = (input_line_opt ic1, input_line_opt ic2) in
    match (s1, s2) with
    | None, None -> Ok ()
    | None, Some str -> Error (error_msg line file_name "EOF" str)
    | Some str, None -> Error (error_msg line file_name str "EOF")
    | Some str1, Some str2 ->
        if str1 = str2 then loop (line + 1)
        else Error (error_msg line file_name str1 str2)
  in
  loop 1

(*first one is actual second one is to_test*)
let tokenized_compare
    (file_name : string)
    (tokenizer : Lexing.lexbuf -> 'a)
    (eqfn : 'a -> 'a -> bool)
    (end_token : 'a)
    (str_of_tok : 'a -> string)
    (ic1 : in_channel)
    (ic2 : in_channel) =
  let lb1 = Lexing.from_channel ic1 in
  let lb2 = Lexing.from_channel ic2 in
  let rec aux b1 b2 =
    let t1 = tokenizer b1 in
    let t2 = tokenizer b2 in
    if eqfn t1 end_token && eqfn t2 end_token then Ok ()
    else if not (eqfn t1 t2) then
      Error
        (error_msg (Lexing.lexeme_start_p b1).pos_lnum (file_name ^ ".expected")
           (str_of_tok t1) (str_of_tok t2))
    else aux b1 b2
  in
  aux lb1 lb2
