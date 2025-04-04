type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  [@@deriving sexp]

type t =
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | LambdaRec of string * string list * t
  | Call of t * t list
  | Prim of prim
  [@@deriving sexp]

(** Parses string [s] into [Intro.t] S-exp, assumes one expression only *)
val parse : string -> t
