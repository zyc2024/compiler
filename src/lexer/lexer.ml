include Tokenizer
open Util

let record_time time_ref f x =
  let start = Sys.time () in
  let y = f x in
  let now = Sys.time () in
  time_ref := !time_ref +. (1000.0 *. (now -. start));
  y

let lex ?(handle = `No_print) lexer =
  (* the time is recorded in ms*)
  let handle_time = ref Float.zero in
  let print_line fmt pos s =
    let l, c = Position.coord_of_pos pos in
    Format.fprintf fmt "%d:%d %s\n" l c s
  in
  let rec loop () =
    match tokenize lexer with
    | token -> begin
        let startp, _endp = get_positions lexer in
        let handler = function
          | `No_print -> ()
          | `Print fmt -> print_line fmt startp (Parse.string_of_token token)
        in
        match token with
        | EOF -> Ok !handle_time
        | _ ->
            record_time handle_time handler handle;
            loop ()
      end
    | exception Lexical_error (pos, msg) ->
        record_time handle_time
          (function
            | `No_print -> () | `Print fmt -> print_line fmt pos ("error:" ^ msg))
          handle;
        Error (!handle_time, pos, msg)
  in
  loop ()

let make_token_generator lexer () =
  let token = tokenize lexer in
  let startp, endp = get_positions lexer in
  (token, startp, endp)

(* let make_token_generator2 tokens = let tok_ref = ref tokens in fun () ->
   match !tok_ref with | [] -> (Parse.EOF, Lexing.dummy_pos, Lexing.dummy_pos) |
   t :: lst -> tok_ref := lst; t *)

(* let string_of_token token_info = let token, startp, _endp = token_info in let
   l, c = Util.Position.coord_of_pos startp in Printf.sprintf "%d:%d %s\n" l c
   (Parse.string_of_token token) *)

(* let print_token fmt token_info = let token, _, _ = token_info in if token =
   Parse.EOF then () else Format.fprintf fmt "%s" (string_of_token
   token_info) *)
