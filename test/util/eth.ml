let input_line_opt ic = try Some (input_line ic) with End_of_file -> None

let error_msg line file_name s1 s2 =
  Printf.sprintf
    "Mismatch detected at line %d of file %s\n\texpected: %s\n\tfound: %s" line
    file_name s1 s2

let error_msg2 line col file_name s1 s2 =
  Printf.sprintf
    "Mismatch detected at %d:%d of file %s\n\texpected: %s\n\tfound: %s" line
    col file_name s1 s2

let compare file_name ~expected ~input =
  let rec loop line =
    let s1, s2 = (input_line_opt expected, input_line_opt input) in
    match (s1, s2) with
    | None, None -> Ok ()
    | None, Some str -> Error (error_msg line file_name "EOF" str)
    | Some str, None -> Error (error_msg line file_name str "EOF")
    | Some str1, Some str2 ->
        if str1 = str2 then loop (line + 1)
        else Error (error_msg line file_name str1 str2)
  in
  loop 1

let compare_sexp file_name ~expected ~input =
  let open Lexer in
  let lexbuf1 = Sedlexing.Utf8.from_channel expected in
  let lexbuf2 = Sedlexing.Utf8.from_channel input in
  let rec loop () =
    let t1, t2 = (Lexer.tokenize lexbuf1, Lexer.tokenize lexbuf2) in
    match (t1, t2) with
    | EOF, EOF -> Ok ()
    | t1, t2 when t1 = t2 -> loop ()
    | _ ->
        let position, _ = Sedlexing.lexing_positions lexbuf2 in
        let l, c =
          (position.pos_lnum, position.pos_cnum - position.pos_bol + 1)
        in
        Error
          (error_msg2 l c file_name (string_of_token t1) (string_of_token t2))
  in
  loop ()
