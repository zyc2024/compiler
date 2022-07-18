(** [Exceeded_maximum_int s] is an exception raised when a lexer matches a
    numerical string [s] which corresponds to an integer out of supported
    bounds. *)
exception Exceeded_maximum_int of string

(** [Invalid_escape s] is an exception raised when a lexer matches an invalid
    escape code *)
exception Invalid_escape of string

(** [Unclosed_literal s] is an exception raised when a string/character is not
    closed on its line. The string [s] gives some detail about the error.*)
exception Unclosed_literal of string

(** [Unsupported_code_point s] is an exception raised when the hexadecimal
    sequence [s] is not within the ranges of 0x0 ... 0xD7FF or 0xE000 ...
    0x10FFFF. *)
exception Unsupported_code_point of string

exception Invalid_character

(** [init ()] starts the lexer in its initial state. This must be done at the
    start of lexing any file. *)
val init : unit -> unit

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
      when an empty character literal or multi-character literal is matched. *)
val tokenize : Sedlexing.lexbuf -> Parse.Parser.token
