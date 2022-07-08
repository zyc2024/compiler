exception DirectoryNotExists of string
exception FileNotExists of string
exception NameTakenByFile of string
exception NameTakenByDirectory of string

let cwd = Sys.getcwd ()

let mkdir_if_nonexistent dir =
  try if Sys.is_directory dir then () else raise (NameTakenByFile dir)
  with Sys_error _ -> ( match Sys.command ("mkdir " ^ dir) with _ -> ())

let open_reader file_name =
  if Sys.file_exists file_name then
    if Sys.is_directory file_name then raise (NameTakenByDirectory file_name)
    else open_in file_name
  else raise (FileNotExists file_name)

let close_reader ic = close_in ic

let open_writer file_name =
  if Sys.file_exists file_name then
    if Sys.is_directory file_name then raise (NameTakenByDirectory file_name)
    else open_out file_name
  else open_out file_name

let close_writer oc = close_out oc
let file_exists dir = Sys.file_exists dir && not (Sys.is_directory dir)
let directory_exists dir = try Sys.is_directory dir with Sys_error _ -> false
