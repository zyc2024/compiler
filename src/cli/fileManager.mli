exception DirectoryNotExists of string
exception FileNotExists of string
exception NameTakenByFile of string
exception NameTakenByDirectory of string

(** [cwd] is the current working directory of the running process. All other
    functions in this module assume this is the starting point for
    reading/writing to files/directories. *)
val cwd : string

(** [mkdir_if_nonexistent dir] creates the directory with name [dir] if
    possible. Raises {!NameTakenByFile} if a file exists with name [dir]. *)
val mkdir_if_nonexistent : string -> unit

(** [open_reader file_name] is an input channel to file with name [file_name].
    Raises {!FileNotExists} if a file with name [file_name] cannot be found.
    Raises {!NameTakenByDirectory} if a directory with name [file_name] exists. *)
val open_reader : string -> in_channel

(** [close_reader ic] closes the input channel [ic]*)
val close_reader : in_channel -> unit

(** [open_writer file_name] is an output channel to file with name [file_name].
    Raises {!NameTakenByDirectory} if [file_name] is the name of an existing
    directory. If the file with name [file_name] cannot be found, the file is
    created.*)
val open_writer : string -> out_channel

(** [close_writer oc] closes and flushes the output channel [oc]. *)
val close_writer : out_channel -> unit

(** [file_exists dir] is true if a file with name [dir] exists. *)
val file_exists : string -> bool

(** [directory_exists dir] is true if a directory with name [dir] exists. *)
val directory_exists : string -> bool