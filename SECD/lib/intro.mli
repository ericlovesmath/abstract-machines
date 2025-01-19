type t =
  | Nil
  | Int of int
  | Var of string
  | Call of t * t list

val parse : string -> t option
val pp : t -> string
