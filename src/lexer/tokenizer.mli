(** [Lexical_error pos s] is an exception raised at file position [pos] when the
    lexer rejects parts of the input lexer buffer. [s] details the error. *)
exception Lexical_error of Lexing.position * string

(** the abstract type for a lexer *)
type t

(** [make_lexer lexbuf] is a lexer in its initial state created from the lexer
    buffer [lexbuf]. The lexer created should only be used for the corresponding
    lexer buffer.

    Requires: [lexbuf] is a fresh lexer buffer. Sedlex buffers are imperative
    constructs and hence the lexer is not a persistent structure. Creating a
    lexer from used lexer buffers may corrupt token positions. *)
val make_lexer : Sedlexing.lexbuf -> t

(** [tokenize lexbuf] is a token corresponding to the matched string in the
    lexer buffer [lexbuf].

    @raise Lexical_error
      when the matched string does not correspond to a valid token. *)
val tokenize : t -> Parse.token

(** [get_position t] is the point in the source file that corresponds to the
    start of the most recently matched token. *)
val get_position : t -> Lexing.position

(** [get_positions t] are the two points in the source file that corresponds to
    the start and end of the most recently matched token. *)
val get_positions : t -> Lexing.position * Lexing.position
