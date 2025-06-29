type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  | Error of string
  [@@deriving sexp]

type t =
  | Unit
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

type top =
  | Define of string * t
  | Expr of t
  [@@deriving sexp]

(** Parses [Intro.top] into the shared frontend AST that all backends expect *)
val desugar_top : Intro.top -> top
