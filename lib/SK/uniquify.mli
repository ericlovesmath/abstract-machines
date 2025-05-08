type t =
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | Lambda of string list * t
  | Y
  | If
  | App of t * t
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

val uniquify : Frontend.Ast.t -> t
