{
    type tok = 
    | StartList | EndList | End | Atomic of string | Failed ;;

    let str_of_tok = function
    | StartList -> "("
    | EndList -> ")"
    | End -> "EOF"
    | Atomic s -> "atom " ^ s 
    | Failed -> "failed?"
}

let ws =  '\r' | ' ' | '\t' 

rule parseParsed = parse 
    | '(' {StartList}
    | ')' {EndList}
    | ws {parseParsed lexbuf}
    | '\n' {Lexing.new_line lexbuf ; parseParsed lexbuf}
    | eof {End}
    | [^'\n''\r'' ''\t''('')']+ as k {Atomic(k)}
    | _ {Failed}

