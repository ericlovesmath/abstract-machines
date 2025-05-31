type t =
  | S | K | Y | C | B | I | U | P
  | If
  | Int of int
  | Bool of bool
  | Var of string
  | Unit
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
  [@@deriving sexp]

val graphify : ((string * Graph.vertex) list * Graph.graph) -> t -> Graph.t
