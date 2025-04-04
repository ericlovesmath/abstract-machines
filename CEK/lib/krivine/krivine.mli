type t =
  | Access of string
  | Grab of string * t
  | Push of t * t
  | If of t * t * t
  | Cst of constant
  [@@deriving sexp]

and constant =
  | Nil
  | Cons of t * t Lazy.t
  | Int of int
  | Bool of bool
  | Prim of Intro.prim
  [@@deriving sexp]

type value = Cl of t * env
  [@@deriving sexp]
and env = (string * value) list

(** Evaluate using Krivine Machine, result in WHNF *)
val eval : t -> value

(** Forces evaluation of WHNF to Normal Form *)
val force : value -> constant

(** Converts output of [force . result] to string *)
val string_of_value : constant -> string
