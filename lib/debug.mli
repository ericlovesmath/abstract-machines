(** Enables/Disables [print_pass] and [trace] debug printing *)
val debug : bool ref

(** Prints [label] and [sexp] if [debug] is on *)
val print_pass : string -> Sexplib.Sexp.t -> unit

(** Utility function, pipeable [print_pass] *)
val trace : string -> ('a -> Sexplib.Sexp.t) -> 'a -> 'a
