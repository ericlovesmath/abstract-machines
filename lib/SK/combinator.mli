type t =
  | S | K | Y | C | B | I | U | P
  | If
  | Int of int
  | Bool of bool
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
  [@@deriving sexp]

val graphify : t -> Graph.t
