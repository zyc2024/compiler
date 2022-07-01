# Lexer

## How to Use
In order to parse the actual lexer, invoke Lexer.evo_lex
During Lexing, the follows exceptions may arise:
 - `UnexpectedChar of string` : Raised when a unidentified character is read
 - `InvalidCharacterLiteral of string` : Raised when a character literal has more than one character in it (ex: 'aa' and '' would both raise InvalidCharacterLiteral)
 - `Invalid Escape` : Raised when a string or character literal contains an unrecognized escape sequence. Recognized escape sequences include `\n`, `\r`, `\t`, `\uHHHH`
 - `MissingEndQuote` : Raised when an end-of-line is found before an expected ' or " in a character or string. 
 - `ExceededMaximumInt` : Raised when an integer exceeds the maximum range. The maximum range is 



Note that Lexer also has some functions that may be of use when it comes to encoding/decoding UTF8. These functions are as follows:
 - `e6b` : Given a string s and an index i, return the last six bits of s[i]
 - `ucharp_to_bytes` : Given a valid utf8 codepoint i, return a byte sequence representing the utf8 encoding of i. This is used in converting "\u0041" to "A". 
 - `str_to_ucharp_list` : Given a string, return a representation of the input string as a list of unicode code points. 
 - `print_uchar_list` : Print a list of given Uchar.t to stdout. 

Other debugging things:
 - `lexing_char` : an int ref; if it is ref 0, then the lexer is not lexing a character. If it is ref 1, then it is.
 - `set_lexing_char` : set whether ro not the lexer is lexing a character based on a boolean
 - `string_buffer` : a buffer for holding the last (if not parsing a string) or the current (if parsing a string) string. The maximum length of a string is 256 bytes.
 - `get_string_buffer` : returns the contents of the string buffer
 - `storec_in_buffer` : store a character in the buffer
 - `stores_in_buffer` : store a string ni the buffer

Lexer.LexUtil has utilities for debugging and helpers. Its contents are as follows:
 - `string_of_token` : Given a Parse.Parser token, return a string representation of the token
 - `handle<ExceptionName>` : handles \<ExceptionName> by printing a message to stderr and exits with exit code 1
 - `outout_tokens` : Given an in_channel and an out_channel, lex the input channel and write thetokens and their locations in the output channel. Note that this method invokes handle\<ExceptionName> and exits if a lexing error is found

### TODO: 

expand maximum string size

more graceful handling of errors