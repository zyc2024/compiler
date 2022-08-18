(** [string_of_unicode c] is the escaped string representation of an unicode
    character whose unicode representation code is [c]. *)
let string_of_unicode code =
  if code >= 128 then
    Printf.sprintf "{%06x}" code |> String.uppercase_ascii |> ( ^ ) "\\x"
  else Uchar.to_char (Uchar.of_int code) |> String.make 1 |> String.escaped

(** [string_of_intq q] transforms a valid int Queue.t, q, representing a unicode
    string into a printable string *)
let string_of_intq q =
  let s = Buffer.create 16 in
  Queue.iter (fun x -> Buffer.add_string s (string_of_unicode x)) q;
  let tor = Buffer.contents s in
  Buffer.clear s;
  tor
