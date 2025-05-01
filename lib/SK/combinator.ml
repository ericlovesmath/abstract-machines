open Sexplib.Std

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

let graphify (ast : t) : Graph.t =
  let g : Graph.graph = Hashtbl.create 32 in

  (* Fixed nodes so they're not recreated each time *)
  let node_s = Graph.add_vertex g S in
  let node_k = Graph.add_vertex g K in
  let node_i = Graph.add_vertex g I in
  let node_y = Graph.add_vertex g Y in
  let node_c = Graph.add_vertex g C in
  let node_b = Graph.add_vertex g B in
  let node_u = Graph.add_vertex g U in
  let node_p = Graph.add_vertex g P in
  let node_if = Graph.add_vertex g If in
  let node_true = Graph.add_vertex g (Bool true) in
  let node_false = Graph.add_vertex g (Bool false) in
  let node_nil = Graph.add_vertex g Nil in
  let node_prims =
    List.map
      (fun p -> (p, Graph.add_vertex g (Prim p)))
      [ Atom; Cons; Cdr; Car; Add; Sub; Mul; Div; Eq; Gt; Lt; Ge; Le ]
  in

  let rec aux (ast : t) : Graph.vertex =
    match ast with
    | S -> node_s
    | K -> node_k
    | I -> node_i
    | Y -> node_y
    | C -> node_c
    | B -> node_b
    | U -> node_u
    | P -> node_p
    | If -> node_if
    | Bool true -> node_true
    | Bool false -> node_false
    | Nil -> node_nil
    | Prim p -> List.assoc p node_prims
    | Int i -> Graph.add_vertex g (Int i)
    | Cons (h, t) -> Graph.add_vertex g (Cons (aux h, aux t))
    | App (f, x) -> Graph.add_vertex g (App (aux f, aux x))
  in
  let entry = aux ast in
  (entry, g)
