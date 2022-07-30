open Format

let add_space = ref false
let print_space_if_needed fmt = if !add_space then pp_print_space fmt () else ()

let print_atom fmt atom =
  print_space_if_needed fmt;
  pp_print_string fmt atom;
  (* the next element in the current sexp tree level needs a space from the
     previous sexp *)
  add_space := true

let start_list fmt () =
  print_space_if_needed fmt;
  pp_open_hvbox fmt 2;
  pp_print_string fmt "(";
  (* the first element following a list doesn't need a space *)
  add_space := false

let end_list fmt () =
  pp_close_box fmt ();
  pp_print_string fmt ")";
  (* a space is needed so the next meanningful item (atom list, atom) doesn't
     appear like ...)<item> *)
  add_space := true
