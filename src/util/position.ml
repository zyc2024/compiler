open Lexing

(** [coord_of_pos pos] is the text file locatio [(line,column)] pair
    corresponding to [pos].*)
let coord_of_pos position =
  (position.pos_lnum, position.pos_cnum - position.pos_bol + 1)

let print_pos position =
  let l, c = coord_of_pos position in
  Printf.fprintf stdout "%d:%d\n" l c
