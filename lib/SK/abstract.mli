type t =
  | Var of string
  | S | K | Y | C | B | I | U | P | If
  | Int of int
  | Bool of bool
  | Unit
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
[@@deriving sexp]

val abstract : Simplify.t -> Combinator.t
