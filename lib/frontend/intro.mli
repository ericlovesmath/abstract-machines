type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  | And | Or | Not | Neq
  [@@deriving sexp]

type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | List of t list
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | Let of string * string list * t * t
  | LetRec of string * string list * t * t
  | LetStar of (string * string list * t) list * t
  | LetRecStar of (string * string list * t) list * t
  | Call of t * t list
  | Prim of prim
  [@@deriving sexp]

(** Parses string [s] into [Intro.t] S-exp, assumes one expression only *)
val parse : string -> t
