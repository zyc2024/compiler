exception Lexical_error of Lexing.position * string

(** [lex_no_output ic] runs lexical analysis on the file data provided by input
    channel [ic].

    @raise Lexical_error when the lexer encounters unsupported lexical syntax. *)
val lex_no_output : in_channel -> unit

(** [lex_with_output ic oc] runs lexical analysis on the file data provided by
    input channel [ic] and prints tokens to output channel [oc]. Printing stops
    at EOF or after a lexical error is encountered.

    @raise Lexical_error when the lexer encounters unsupported lexical syntax. *)
val lex_with_output : in_channel -> out_channel -> unit
