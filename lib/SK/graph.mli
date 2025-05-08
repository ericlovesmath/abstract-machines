open Sexplib.Std

type vertex = int [@@deriving sexp]

type label =
  | S | K | Y | C | B | I | U | P
  | If
  | Int of int
  | Bool of bool
  | Nil
  | Cons of vertex * vertex
  | Prim of Frontend.Ast.prim
  | App of vertex * vertex
  [@@deriving sexp]

type graph = (vertex, label) Hashtbl.t [@@deriving sexp]
type t = vertex * graph [@@deriving sexp]

val add_vertex : graph -> label -> vertex
val string_of_t : t -> string
val reduce : t -> t
