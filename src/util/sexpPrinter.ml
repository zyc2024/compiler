open Format

type printer = {
  add_space : bool ref;
  fmt : Format.formatter;
}

let make_printer fmt = { add_space = ref false; fmt }

let print_space_if_needed p =
  if !(p.add_space) then pp_print_space p.fmt () else ()

let print_atom p atom =
  print_space_if_needed p;
  pp_print_string p.fmt atom;
  (* the next element in the current sexp tree level needs a space from the
     previous sexp *)
  p.add_space := true

let start_list p () =
  print_space_if_needed p;
  pp_open_hvbox p.fmt 2;
  pp_print_string p.fmt "(";
  (* the first element following a list doesn't need a space *)
  p.add_space := false

let end_list p () =
  pp_close_box p.fmt ();
  pp_print_string p.fmt ")";
  (* a space is needed so the next meanningful item (atom list, atom) doesn't
     appear like ...)<item> *)
  p.add_space := true
