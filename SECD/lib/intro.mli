type prim = Add | Sub | Mul | Div

type t =
  | Nil
  | Int of int
  | Var of string
  | Call of t * t list
  | Prim of prim

val parse : string -> t option
val pp : t -> string
