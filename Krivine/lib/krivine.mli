type t =
  | Var of string
  | Lambda of string * t
  | App of t * t
  | Int of int
  | Bool of bool
  | If of t * t * t
  | Add | Sub | Mul | Div
  | Eq | Lt | Gt | Le | Ge
  | Nil | Cons | Head | Tail | IsEmpty

type closure =
  | Closure of t * (string * closure) list
  | IntVal of int
  | BoolVal of bool
  | ListNil
  | ListCons of closure * closure
  | PrimOp of prim_op

and prim_op =
  | PAdd | PSub | PMul | PDiv
  | PEq | PLt | PGt | PLe | PGe
  | PCons | PHead | PTail | PIsEmpty

(** Evaluate using Krivine Machine, result in WHNF *)
val result : t -> closure

(** Force WHNF to HNF *)
val force : closure -> closure

(** Converts output of [force . result] to string *)
val string_of_value : closure -> string
