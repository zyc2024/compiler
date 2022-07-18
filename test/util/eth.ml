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
