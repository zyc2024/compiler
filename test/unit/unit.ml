(** collection of utility useful for unit testing. *)

open OUnit2

(** [make_test name expected f input ~printer ~cmp] is an OUnit test with label
    [name] which compares [f input] against [expected]. In case of a mismatch,
    the values are printed using the given [printer]. To determine whether
    values are equal, the comparator [cmp] is used. In most cases, [cmp] can be
    [( = )]*)
let make_test name expected func input ~printer ~cmp =
  name >:: fun _ -> assert_equal expected (func input) ~printer ~cmp

(** [pp_string s] pretty prints the string [s] with surrounding quotations and
    characters escaped. *)
let pp_string s = Printf.sprintf "\"%s\"" (String.escaped s)
