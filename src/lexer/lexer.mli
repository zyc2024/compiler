include module type of Tokenizer

(** [lex ?handle lexer] is [Ok (handle_time)] if lexical analysis successfully
    completes (tokenization never fails up to [EOF]) on the given lexer,
    otherwise [Error (position, message)]. The default action handler on
    produced tokens is [`No_print]. *)
val lex :
  ?handle:[< `Print of Format.formatter | `No_print > `No_print ] ->
  t ->
  (float, float * Lexing.position * string) result

(** [make_token_generator lexer] is a generator function
    [() -> token, start_position, end_position] *)
val make_token_generator : t -> unit -> Parse.token_info

(** [make_token_generator token_list] is similar to [make_token_generator lexer]
    but the generator function [() -> token, start_position, end_position] is
    derived from a token list. *)
(* val make_token_generator2 : Parse.token_info list -> unit -> token_info *)

(** [string_of_token (token, startp, endp)] is the string representation of a
    token using its start position [startp]. *)
(* val string_of_token : Parse.token_info -> string *)

(* val print_token : Format.formatter -> token_info -> unit *)