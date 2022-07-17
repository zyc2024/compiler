exception MissingPositionalArgument of string
exception IncorrectFileExtension of string
exception UnsupportedFlag of string

type spec = {
  name : string;
  arg_required : bool;
  description : string;
  alternatives : string list;
  verifier : string -> unit;
  arg_name : string;
}

let no_verify (_ : string) = ()

let make_spec name ?(alternatives = []) ~description ~arg verifier =
  {
    name;
    arg_required = true;
    description;
    alternatives;
    verifier;
    arg_name = arg;
  }

let make_spec_no_arg name ?(alternatives = []) description =
  {
    name;
    arg_required = false;
    description;
    alternatives;
    verifier = no_verify;
    arg_name = String.empty;
  }

let description spec = spec.description
let name spec = spec.name
let names spec = spec.name :: spec.alternatives

let compare_name spec1 spec2 =
  let open String in
  let str_without_dash str =
    List.find (fun s -> not (equal empty s)) (split_on_char '-' str)
  in
  compare (str_without_dash spec1.name) (str_without_dash spec2.name)

let spec_usage spec =
  if spec.arg_required then
    List.fold_left
      (fun acc elem -> Printf.sprintf "%s, %s <%s>" acc elem spec.arg_name)
      (Printf.sprintf "%s <%s>" spec.name spec.arg_name)
      spec.alternatives
  else
    List.fold_left
      (fun acc elem -> Printf.sprintf "%s, %s" acc elem)
      spec.name spec.alternatives

module StringMap = Map.Make (String)

type result = {
  files : string list;
  enabled : (string * string) list;
}

(* string utility (replaced by 4.12+)*)
(* let starts_with (prefix : string) (s : string) = try let len = String.length
   prefix in let substr = String.sub s 0 len in String.equal substr prefix with
   Invalid_argument _ -> false *)

(* let ends_with (suffix : string) (s : string) = try let len = String.length
   suffix in let substr = String.sub s (String.length s - len) len in
   String.equal substr suffix with Invalid_argument _ -> false *)

(* parse the given arguments recursively. If the given argument is not a flag,
   immediately begin processing the arguments as files *)
let rec parse_aux args spec_map name_map result =
  match args with
  | [] -> result
  | arg :: lst ->
      (* check if the user denoted the end of options *)
      if String.equal "--" arg then parse_files lst result
      else if String.starts_with ~prefix:"-" arg then
        if StringMap.mem arg name_map then
          (* the user passed in a supported flag *)
          parse_flag (StringMap.find arg name_map) lst spec_map name_map result
        else raise (UnsupportedFlag arg)
      else parse_files args result

and parse_flag flag args spec_map name_map result =
  let spec = StringMap.find flag spec_map in
  if spec.arg_required then (
    (* supported flag requires argument *)
    match args with
    | [] -> raise (MissingPositionalArgument flag)
    | flagArg :: lst ->
        spec.verifier flagArg;
        parse_aux lst spec_map name_map
          { result with enabled = (flag, flagArg) :: result.enabled })
  else
    parse_aux args spec_map name_map
      { result with enabled = (flag, String.empty) :: result.enabled }

and parse_files (args : string list) (result : result) =
  match args with
  | [] -> result
  | arg :: lst -> parse_files lst { result with files = arg :: result.files }

let rec make_spec_map specs map =
  match specs with
  | [] -> map
  | spec :: t -> make_spec_map t (StringMap.add spec.name spec map)

let rec make_name_map specs map =
  match specs with
  | [] -> map
  | spec :: t ->
      make_name_map t
        (List.fold_left
           (fun m elem -> StringMap.add elem spec.name m)
           map spec.alternatives
        |> StringMap.add spec.name spec.name)

let parse (sys_args : string list) (specs : spec list) =
  (* convert specs into a spec map for convenience *)
  let spec_map = make_spec_map specs StringMap.empty in
  (* save all alternatives *)
  let name_map = make_name_map specs StringMap.empty in
  parse_aux (List.tl sys_args) spec_map name_map { files = []; enabled = [] }

(* O(n) performance but remedied if the list value will be iterated. *)
let flag_and_args parsed = List.rev parsed.enabled

let files (parsed : result) (exts : string list) =
  let inputs = List.rev parsed.files in
  let has_valid_ext file =
    List.fold_left
      (fun acc elem -> acc || String.ends_with ~suffix:elem file)
      false exts
  in
  match List.filter (fun file -> not (has_valid_ext file)) inputs with
  | [] -> inputs
  | file_arg :: _ -> raise (IncorrectFileExtension file_arg)