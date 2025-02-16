type t =
  | Int of int
  | Bool of bool
  | Add of t * t
  | Lt of t * t
  | Var of string
  | Let of string * t * t
  | If of t * t * t
  | Fn of string list * t
  | Rec of string * string list * t
  | Call of t * t list

type value

(** Evaluate using CEK Machine, assumes input in ANF *)
val eval : t -> value

(** Converts output of [eval] to string *)
val string_of_value : value -> string
