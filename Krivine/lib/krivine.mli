type t =
  | Var of string
  | Lambda of string * t
  | App of t * t
  | Nil
  | Int of int
  | Bool of bool
  | If of t * t * t
  | Prim of Intro.prim

type value =
  | Closure of t * (string * value) list
  | IntVal of int
  | BoolVal of bool
  | ListNil
  | ListCons of value * value
  | PrimOp of Intro.prim

(** Evaluate using Krivine Machine, result in WHNF *)
val eval : t -> value

(** Forces evaluation of WHNF to Normal Form *)
val force : value -> value

(** Converts output of [force . result] to string *)
val string_of_value : value -> string
