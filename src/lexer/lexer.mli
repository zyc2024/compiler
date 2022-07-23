(** [Exceeded_maximum_int] is an exception raised when the lexer scans a number
    which is not within the supported range of the 64-bit integer. *)
exception Exceeded_maximum_int

(** [Invalid_escape s] is an exception raised when the lexer scans an invalid
    escape code *)
exception Invalid_escape of string

(** [Unclosed_literal s] is an exception raised when a scanned string/character
    is not closed on its line. The string [s] specifies the type of the literal.*)
exception Unclosed_literal of string

(** [Unsupported_code_point seq] is an exception raised when the scanned
    hexadecimal sequence [seq] is not within the ranges of 0x0 ... 0xD7FF or
    0xE000 ... 0x10FFFF. *)
exception Unsupported_code_point of string

(** [Invalid_character] is an exception raised when a scanned single quoted
    sequence does not match the syntax of a valid character literal. *)
exception Invalid_character

(** [Illegal_character] is an exception raised when non ascii characters are
    used out of the context of a sequence literal. *)
exception Illegal_character

(** the type for a lexer *)
type t

(** [make_lexer lexbuf] is a lexer in its initial state created from the lexer
    buffer [lexbuf]. The lexer created should only be used for the corresponding
    lexer buffer. *)
val make_lexer : Sedlexing.lexbuf -> t

(** [tokenize lexbuf] is a token corresponding to the matched string in the
    lexer buffer [lexbuf].

    @raise Exceeded_maximum_int
      when the matched string is a numerical literal that cannot be represented
      in the range of supported integers.
    @raise Invalid_escape
      when the matched escaped sequence does not comply with the syntax [\uHHHH]
      or [\xH\{1,6\}] and other common escaped characters such as [\n,\t]
    @raise Unclosed_literal
      when a string or character literal is not closed on the line where it was
      started.
    @raise Unsupported_code_point
      when a provided unicode character is out of the accepted ranges of
      [0 ... D7FF] and [E000 ... 10FFFF]
    @raise Invalid_character
      when an empty character literal or multi-character literal is matched.
    @raise Illegal_character when non ascii characters are used out of context *)
val tokenize : t -> Parse.Parser.token

(** [get_position t] is the point in the source file that corresponds to the
    start of the most recently matched token. *)
val get_position : t -> Lexing.position
