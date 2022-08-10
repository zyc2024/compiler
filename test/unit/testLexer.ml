(** Apporach: This test script uses strings as input to lexer buffers and
    verifies the produced lexical tokens. *)

open OUnit2
open Lex
open Unit

let test_buffer = Buffer.create 32

(** [lex_from_string s] is the string output from tokenizing the stream of
    unicode characters constituting [s].*)
let lex_from_string s =
  Buffer.reset test_buffer;
  (* for whatever reason, the first matched token for strings don't end up at
     1:1 so we need to set the starting position there. *)
  let lexbuf = Sedlexing.Utf8.from_string s in
  let starting_pos, _ = Sedlexing.lexing_positions lexbuf in
  Sedlexing.set_position lexbuf { starting_pos with pos_lnum = 1 };
  let fmt = Format.formatter_of_buffer test_buffer in
  let _ = lex_with_output lexbuf fmt in
  Format.pp_print_flush fmt ();
  Buffer.contents test_buffer

(* note that for sake of shortening the strings in tests, DO NOT terminate the
   [expected] output string with a newline. *)
let make name ~expected ~input =
  make_test name (expected ^ "\n") lex_from_string input ~printer:pp_string
    ~cmp:( = )

let lex_only name ~expected ~input =
  let lex s =
    let lexbuf = Sedlexing.Utf8.from_string s in
    let starting_pos, _ = Sedlexing.lexing_positions lexbuf in
    Sedlexing.set_position lexbuf { starting_pos with pos_lnum = 1 };
    match lex_no_output lexbuf with
    | Ok () -> "success"
    | Error _ -> "lexical error"
  in
  name >:: fun _ -> assert_equal expected (lex input) ~printer:pp_string

let tests =
  [
    make "lex id" ~expected:"1:1 id x" ~input:"x";
    make "lex ID" ~expected:"1:2 id X" ~input:" X";
    make "lex number" ~expected:"1:1 integer 100" ~input:"100";
    make "lex big integers" ~expected:"2:1 error:integer number too large"
      ~input:"\n10000000000000000000000";
    make "lex string" ~expected:"1:1 string \"hello world\""
      ~input:"\"hello world\"";
    make "lex quote in string" ~expected:"1:1 string \"'\"" ~input:"\"'\"";
    make "lex character" ~expected:"1:1 character 'X'" ~input:"'X'";
    make "lex empty char" ~expected:"1:1 error:invalid character literal"
      ~input:"''";
    make "lex long char" ~expected:"1:1 error:invalid character literal"
      ~input:"'hello world'";
    make "lex caml char" ~expected:"1:1 character '\\x{01F42B}'" ~input:"'🐫'";
    make "lex double quote char" ~expected:"1:1 character '\\\"'" ~input:"'\"'";
    make "random caml" ~expected:"1:1 error:illegal character" ~input:"🐫";
    make "unclosed char eof" ~expected:"1:1 error:unclosed character literal"
      ~input:"'";
    make "unclosed str eof" ~expected:"1:3 error:unclosed string literal"
      ~input:"  \"";
    make "unclosed char newline"
      ~expected:"1:1 error:unclosed character literal" ~input:"'\n";
    make "unclosed str newline" ~expected:"1:1 error:unclosed string literal"
      ~input:"\"\n";
    make "escape newline char literal" ~expected:"1:1 character '\\n'"
      ~input:"'\\n'";
    make "escape carriage return char literal" ~expected:"1:1 character '\\r'"
      ~input:"'\\r'";
    make "escape tab char literal" ~expected:"1:1 character '\\t'"
      ~input:"'\\t'";
    make "escape double quote char literal" ~expected:"1:1 character '\\\"'"
      ~input:"'\\\"'";
    make "escape single quote char literal" ~expected:"1:1 character '''"
      ~input:"'\\''";
    make "escape backslash char literal" ~expected:"1:1 character '\\\\'"
      ~input:"'\\\\'";
    make "unicode char" ~expected:"1:1 character '\\x{00FFFF}'"
      ~input:"'\\uffff'";
    make "hex char" ~expected:"1:1 character '\\x{01F42B}'"
      ~input:"'\\x{1f42b}'";
    make "invalid escape hex form"
      ~expected:"1:2 error:invalid character escape \\x" ~input:"'\\xwhatever'";
    make "invalid codepoint"
      ~expected:"1:3 error:BEEF24 is not a supported code point"
      ~input:"'a\\x{Beef24}'";
    make "lex tokens after strings" ~expected:"1:1 string \"a\"\n2:1 integer 1"
      ~input:"\"a\"\n1";
    lex_only "no-print success run" ~expected:"success" ~input:"x";
    lex_only "no_print failed run" ~expected:"lexical error" ~input:"?";
  ]

let suite = "test suite for lexer" >::: tests
let _ = run_test_tt_main suite
