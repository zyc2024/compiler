open Format

let current_formatter = ref std_formatter
let get_current_formatter () = !current_formatter
let set_formatter fmt = current_formatter := fmt
let flush_contents () = Format.pp_print_flush !current_formatter ()
let add_space = ref false

let print_space_if_needed () =
  if !add_space then pp_print_space !current_formatter () else ()

let print_atom atom =
  print_space_if_needed ();
  pp_print_string !current_formatter atom;
  (* the next element in the current sexp tree level needs a space from the
     previous sexp *)
  add_space := true

let start_list () =
  print_space_if_needed ();
  pp_open_hvbox !current_formatter 2;
  pp_print_string !current_formatter "(";
  (* the first element following a list doesn't need a space *)
  add_space := false

let end_list () =
  pp_close_box !current_formatter ();
  pp_print_string !current_formatter ")";
  (* a space is needed so the next meanningful item (atom list, atom) doesn't
     appear like ...)<item> *)
  add_space := true
