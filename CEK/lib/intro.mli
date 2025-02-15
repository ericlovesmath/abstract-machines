type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le

type t =
  | Nil
  | Int of int
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | Call of t * t list
  | Prim of prim

(** Parses string [s] into [Intro.t] S-exp, assumes one expression only *)
val parse : string -> t option

(** Converts [Intro.t] to string for pretty printing purposes *)
val pp : t -> string
