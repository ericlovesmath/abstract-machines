(** Enables/Disables [debug_print] *)
val debug : bool ref

(** Prints [label] and [sexp] if [debug] is on *)
val debug_print : string -> Sexplib.Sexp.t -> unit
