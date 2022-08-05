type token =
  | LPAREN
  | RPAREN
  | ATOM of string
  | ERROR of string
  | EOF

let string_of_token = function
  | LPAREN -> "("
  | RPAREN -> ")"
  | ATOM s -> s
  | EOF -> "EOF"
  | ERROR e -> "error token " ^ e

let white_space = [%sedlex.regexp? Chars " \t\n\r"]
let atom = [%sedlex.regexp? Plus (Sub (any, (')' | '(' | white_space)))]

let rec tokenize lexbuf =
  match%sedlex lexbuf with
  | "(" -> LPAREN
  | ")" -> RPAREN
  | eof -> EOF
  | atom -> ATOM (Sedlexing.Utf8.lexeme lexbuf)
  | white_space -> tokenize lexbuf
  | _ -> ERROR (Sedlexing.Utf8.lexeme lexbuf)
