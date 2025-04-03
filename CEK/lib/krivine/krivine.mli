type t =
  | Access of string
  | Grab of string * t
  | Push of t * t
  | If of t * t * t
  | Cst of constant

and constant =
  | Nil
  | Cons of t * t Lazy.t
  | Int of int
  | Bool of bool
  | Prim of Intro.prim

and value

(** Evaluate using Krivine Machine, result in WHNF *)
val eval : t -> value

(** Forces evaluation of WHNF to Normal Form *)
val force : value -> constant

(** Converts output of [force . result] to string *)
val string_of_value : constant -> string
