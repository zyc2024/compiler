

{
    open Parse.Parser
    exception UnexpectedChar of string
    exception InvalidCharacterLiteral of string
    exception InvalidEscape 
    exception MissingEndQuote
    exception ExceededMaximumInt 





    let lexing_char = ref 0
    let set_lexing_char v = if v then lexing_char := 1 else lexing_char := 0;;

    let e6b s i = 0b00111111  land (Char.code (String.get s i)) 

    let string_buffer = Buffer.create 256
    let reset_string_buffer () = Buffer.reset string_buffer
    let get_string_buffer () = Buffer.contents string_buffer

    let storec_in_buffer c = Buffer.add_char string_buffer c
    let stores_in_buffer s = Buffer.add_string string_buffer s


    let ucharp_to_bytes i = 
        if (i >= 0x10000) then
            let b = Bytes.create 4 in               
            (Bytes.set b 2 (Char.chr(0b10000000 + (i land 0b111111))));
            (Bytes.set b 2 (Char.chr(0b10000000 + (i/64 land 0b111111))));
            (Bytes.set b 1 (Char.chr(0b10000000 + ((i/4096) land 0b111111))));
            (Bytes.set b 0 (Char.chr(0b11110000 + ((i/262144) land 0b1111))));
            b
        else
            if (i >= 0x800) then
                let b = Bytes.create 3 in               
                (Bytes.set b 2 (Char.chr(0b10000000 + (i land 0b111111))));
                (Bytes.set b 1 (Char.chr(0b10000000 + ((i/64) land 0b111111))));
                (Bytes.set b 0 (Char.chr(0b11100000 + ((i/4096) land 0b1111))));
                b
            else
                if (i >= 0x80) then
                    let b = Bytes.create 2 in 
                    (Bytes.set b 1 (Char.chr(0b10000000 + (i land 0b111111))));
                    (Bytes.set b 0 (Char.chr(0b11000000 + ((i/64) land 0b11111))));
                    b
                else
                    Bytes.make 1 (Char.chr i) ;;
    let str_to_ucharp_list s = 
        let rec s2ul stri ind =
            if ( (String.length stri) > ind) then
                let b = Char.code (String.get stri ind) in 
                
                if b >=  0b11110000 then
                    (262144*( b land 0b111) + 4096*(e6b stri (ind+1)) + 64*(e6b stri (ind+2)) + (e6b stri (ind)) ) :: (s2ul stri (ind+4))
                else
                    if (b >= 0b11100000) then
                        (4096*( b land 0b1111) + 64*(e6b stri (ind+1)) + (e6b stri (ind+2))) :: (s2ul stri (ind+3))
                    else
                        if (b >= 0b11000000) then
                            (64*(b land 0b11111) + (e6b stri (ind+1) )) :: (s2ul stri (ind+2))
                        else
                            b :: (s2ul stri (ind+1))
            else
                [] in 
    (s2ul s 0) ;;
    
    let rec print_uchar_list = function
        | h :: t -> (Printf.printf "%d, " (Uchar.to_int h) ) ; print_uchar_list t 
        | [] -> print_endline "L"


}


let ws = [' ' '\t' '\r' ]+   (* move the \n and \r out later *)
let int = ['1' - '9'] ['0' - '9']*
let H = ['a' - 'f' 'A' - 'F' '0' - '9']
(* let frac = '.' ['0' - '9']+ *)
let c = "\\n" | "\\r" | "\\t" | "\\\"" | "\\\'" | "\\u" H H H H | _#['\\' '\"' '\''] 

(* a page taken out of the example *)




rule evo_lex = parse
    |  ws  {evo_lex lexbuf}
    | ['\n'] {Lexing.new_line lexbuf ; evo_lex lexbuf}
    | '/' '/' (_#'\n')* {evo_lex lexbuf}
    | '/' '*' {
        tokcom lexbuf;
        evo_lex lexbuf
    }
    |   ( '0' | (['1' - '9'] ['0' - '9']*) | ("0x" H +) ){ 
        try 
            let i = Int64.of_string (Lexing.lexeme lexbuf)  in 
                INT_LIT(i)
        with Failure _ -> raise ExceededMaximumInt
   
    }
    | '-' ws* "9223372036854775808" {
        INT_LIT(Int64.of_string "-9223372036854775808")
    }
    | '\"'  {
        set_lexing_char(false);
        reset_string_buffer();
        tokstr lexbuf;
        let s = get_string_buffer() in
            STR_LIT( s )
    }
    | '\''{
        set_lexing_char(true);
        reset_string_buffer();
        tokstr lexbuf;
        let s = get_string_buffer() in
            match  str_to_ucharp_list s with
                | _::_::_ | [] -> raise (InvalidCharacterLiteral s)
                | h::_ -> CHAR_LIT( h)


    }
    | "false" {
        BOOL_LIT ( false )
    }
    | "true"  {
        BOOL_LIT ( true )
    }

    | "boolean" {BOOL}
    | "int" {INT}
    | "char" {CHAR}
    | "type" {TYPE}
    | "void" {VOID}
    | "const" {CONST}
    | "null" {NULL}
    | "if" {IF}
    | "else" {ELSE}
    | "for" {FOR}
    | "while" {WHILE}
    | "return" {RETURN}
    | "continue" {CONTINUE}
    | "break" {BREAK}
    | "import" {IMPORT}

    | '(' ws ? "int"  ws ? ')'  {CINT}
    | '(' ws ? "char"  ws ? ')'  {CCHAR}
    | ['a' - 'z' 'A' - 'Z']['a' - 'z' 'A' - 'Z' '_' '0'-'9']* as i {ID(i)}
    | '_' {USCORE}
    | '[' {LSBRAC}
    | ']' {RSBRAC}

    | '(' {LPAREN}
    | ')' {RPAREN}
    | '{' {LCBRAC}
    | '}' {RCBRAC}
    | ';' {SCOLON}
    | '.' {PERIOD}
    | ',' {COMMA}

    | "==" {DEQ}
    | "!=" {NEQ}
    | "<=" {LTE}
    | ">=" {GTE}
    | '<' {LT}
    | '>' {GT}



    | '+' {ADD}
    | '-' {SUB}
    | '*' {MUL}
    | '/' {DIV}
    | '%' {MOD}
    | '=' {EQ}
    | '!' {LNOT}
    | "&&" {LAND}
    | "||" {LOR}
    
    | '~' {BNOT}
    | '&' {BAND}
    | '|' {BOR}
    
    | _ as u{
        raise (UnexpectedChar (String.make 1 u) )
    }
    | eof {EOF}
and tokstr = parse 
    | '\"' {if lexing_char = ref 1 then (storec_in_buffer '"' ; tokstr lexbuf )else () }
    | '\'' {if lexing_char = ref 1 then () else (storec_in_buffer '\''; tokstr lexbuf)}
    | "\\n" {storec_in_buffer '\n' ; tokstr lexbuf}
    | "\\r" {storec_in_buffer '\r' ; tokstr lexbuf}
    | "\\t" {storec_in_buffer '\t'; tokstr lexbuf}
    | "\\\"" {storec_in_buffer '\"' ; tokstr lexbuf}
    | "\\\'" {storec_in_buffer '\'' ; tokstr lexbuf}
    | "\\\\" {storec_in_buffer '\\' ; tokstr lexbuf}
    | "\\u" ((H H H H) as hc){
        stores_in_buffer (Bytes.to_string (ucharp_to_bytes (
            int_of_string ("0x" ^ hc)
        )) ) ; tokstr lexbuf
    }
    | '\\' _   { raise InvalidEscape }
    | '\n'   { raise MissingEndQuote }
    | _ as achr { 
        storec_in_buffer achr ; tokstr lexbuf
    }
and tokcom = parse
    | "*/" {()}
    | _ {tokcom lexbuf}