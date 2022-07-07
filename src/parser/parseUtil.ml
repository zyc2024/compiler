


open Parser
open Parser.MenhirInterpreter
open Lexing

let parse_with_first_token_fed lfunc istream ostream =
let lb = Lexing.from_channel istream in
let n = Parser.Incremental.main lb.lex_curr_p in 
let rec consume_tokens tgen b s otok =  match s with
    | Accepted _ -> Printf.fprintf ostream "ACCEPTED\n"; 
    | Rejected -> Printf.fprintf ostream "REJECTED\n"; 
    | HandlingError e -> Printf.fprintf ostream "handle error: short circuit and terminate here\n"; let (spos, _) = positions e in Printf.printf "failed at line %d column %d\n" spos.pos_lnum (spos.pos_cnum - spos.pos_bol + 1); consume_tokens tgen b (resume s) EOF
    | AboutToReduce (_,_) -> consume_tokens tgen b (resume s) EOF
    | Shifting(_,_,_) -> consume_tokens tgen b (resume s) EOF
    | InputNeeded _ -> 
        if (otok != EOF) then
        let ns = offer s (otok, dummy_pos ,  dummy_pos) in consume_tokens tgen b ns Parser.EOF
        else
        let t = (tgen b) in let ns = offer s (t, Lexing.lexeme_start_p b,  Lexing.lexeme_end_p b) in consume_tokens tgen b ns EOF
in consume_tokens lfunc lb n MOD_START;;