type t =
  | S | K | Y | C | B | I | U | P (* TODO: rm *)
  | If
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | Lam of string * t
  | App of t * t
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

val simplify : Uniquify.t -> t
