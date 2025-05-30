type t =
  | Access of string
  | Grab of string * t
  | Push of t * t
  | If of t * t * t
  | Cst of const
  [@@deriving sexp]

and const =
  | Unit
  | Nil
  | Cons of t * t Lazy.t
  | Int of int
  | Bool of bool
  | Prim of Frontend.Ast.prim
  | Closure of closure
  [@@deriving sexp]

and closure = Cl of t * env
  [@@deriving sexp]
and env = (string * closure) list

(** Evaluate using Krivine Machine, result in WHNF *)
val eval : env -> t -> closure

(** Forces evaluation of WHNF to Normal Form *)
val force : closure -> const

(** Converts output of [force . result] to string *)
val string_of_const : const -> string
