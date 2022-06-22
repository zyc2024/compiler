
open Parse.Parser


(* let get_token_list c = 
    let rec loop gfunc stm = 
        let t = (gfunc stm) in 
        (* not changing for now, but this is dangerous! *)
        (* callstack linear with respect to number of tokens *)
        match t with
            | Parse.EOF -> (t, Lexing.lexeme_start_p) :: []
            | _ -> ((t,Lexing.lexeme_start_p) :: (loop gfunc stm)) in
    (* let f = open_in "t.txt" in  *)
    loop Lexer.evo_lex c *)

let string_of_token = function
  | WHILE -> "while"
  | VOID -> "void"
  | SUB -> "-"
  | STR_LIT (s) -> "str " ^ s
  | SCOLON -> ";"
  | RSBRAC -> "]"
  | RPAREN -> ")"
  | RCBRAC -> "}"
  | PERIOD -> "."
  | NULL -> "null"
  | NEQ -> "!="
  | MUL -> "*"
  | MOD -> "%"
  | LTE -> "<="
  | LT -> "<"
  | LSBRAC -> "["
  | LPAREN -> "("
  | LOR -> "||"
  | LNOT -> "!"
  | LCBRAC -> "{"
  | LAND -> "&&"
  | INT_LIT (i) -> "int " ^ (Int64.to_string i)
  | INT -> "int"
  | IMPORT -> "import"
  | IF -> "if"
  | ID (id) -> "id " ^ id
  | GTE -> ">="
  | GT -> ">"
  | FOR -> "for"
  | EQ -> "="
  | EOF -> "EOF"
  | ELSE -> "else"
  | DIV -> "/"
  | DEQ -> "=="
  | CINT -> "(int)"
  | CHAR_LIT (c) -> "char_lit " ^ (string_of_int c)
  | CHAR -> "char"
  | CCHAR -> "(char)"
  | BOOL_LIT (b) -> "bool_lit " ^ (string_of_bool b)
  | BOOL -> "bool"
  | ADD -> "+"
  | RETURN -> "return"
  | CONTINUE -> "continue"
  | BREAK  -> "break"
  | BNOT -> "~"
  | BAND -> "&"
  | BOR -> "|"
  | COMMA -> ","
  | TYPE -> "type"
  | CONST -> "const"
  | USCORE -> "_"




(* type tokresult =  Token of Parse.token| LError of string ;; *)

(* let get_next_token lb = 
    try Token (Lexer.evo_lex lb )with
        | Lexer.UnexpectedChar t -> 
            let p = Lexing.lexeme_start_p lb in
            LError (Printf.sprintf "Unexpected character \"%s\" at %d:%d" t p.pos_lnum (p.pos_cnum - p.pos_bol + 1) )
        | Lexer.ExceededMaximumInt -> 
            let p = Lexing.lexeme_start_p lb in
            LError (Printf.sprintf "Integer literal \"%s\" exceeds maximum limit at %d:%d" (Lexing.lexeme lb) p.pos_lnum (p.pos_cnum - p.pos_bol + 1) )
 *)





let handleUnexpectedChar s lb= 
    let p = Lexing.lexeme_start_p lb in 
    Printf.eprintf "Unexpected character \"%s\" at %d:%d\n" s p.pos_lnum (p.pos_cnum - p.pos_bol + 1) ;
    exit 1 ;;

let handleExceededMaximumInt lb=
    let p = Lexing.lexeme_start_p lb in 
    Printf.eprintf "Integer literal exceeded maximum limit at %d:%d\n"  p.pos_lnum (p.pos_cnum - p.pos_bol + 1) ; 
    exit 1 ;;

    exception InvalidCharacterLiteral
    exception InvalidEscape
    exception BadCharacter

let handleInvalidCharacterLiteral lb s =     
    let p = Lexing.lexeme_start_p lb in 
    Printf.eprintf "Character literal \'%s\' is not one character at %d:%d\n"  (s) p.pos_lnum (p.pos_cnum - p.pos_bol + 1) ; 
    exit 1 ;;

let handleInvalidEscape lb = 
    let p = Lexing.lexeme_start_p lb in 
    Printf.eprintf "Invalid escape sequence \'%s\' at %d:%d\n"  (Lexing.lexeme lb) p.pos_lnum (p.pos_cnum - p.pos_bol + 1) ; 
    exit 1 ;;

let handleMissingEndQuote lb = 
    let p = Lexing.lexeme_start_p lb in 
    Printf.eprintf "Unexpected linebreak at %d:%d (expected \'%c\')\n"  p.pos_lnum (p.pos_cnum - p.pos_bol + 1) (if Lexer.lexing_char = ref 0 then '\"' else '\'' ); 
    exit 1 ;;

let output_tokens istream ostream = 
    (* let rec loop gfunc stm = 
    let t = (gfunc stm) in
    let p = Lexing.lexeme_start_p stm in
    let () = Printf.fprintf ostream "%d:%d %s\\n" p.pos_lnum (p.pos_cnum - p.pos_bol) (string_of_token t) in  *)
    let rec loop gfunc stm = 
    let t = (gfunc stm ) in 
    let p = Lexing.lexeme_start_p stm  in
    try 
        match t with
            | EOF -> flush ostream 
            | _ -> ( let () = (Printf.fprintf ostream "%d:%d %s\n" p.pos_lnum (p.pos_cnum - p.pos_bol + 1) (string_of_token t) )in loop gfunc stm)
    with
        | Lexer.UnexpectedChar s -> handleUnexpectedChar s stm
        | Lexer.ExceededMaximumInt -> handleExceededMaximumInt stm
        | Lexer.InvalidCharacterLiteral s -> handleInvalidCharacterLiteral stm s
        | Lexer.InvalidEscape -> handleInvalidEscape stm
        | Lexer.MissingEndQuote -> handleMissingEndQuote stm
    in loop Lexer.evo_lex (Lexing.from_channel istream)