type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le

type t =
  | Nil
  | Int of int
  | Var of string
  | If of t * t * t
  | Call of t * t list
  | Prim of prim

val parse : string -> t option
val pp : t -> string
