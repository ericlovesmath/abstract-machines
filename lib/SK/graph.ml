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

(* TODO: Make this reset with some kind of [init] *)
let counter = ref 0
let add_vertex (graph : graph) (label : label) : vertex =
  let v = !counter in
  incr counter;
  Hashtbl.add graph v label;
  v

let rec string_of_t ((v, g) : t) : string =
  match Hashtbl.find g v with
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | Y -> "Y"
  | C -> "C"
  | B -> "B"
  | U -> "U"
  | P -> "P"
  | Int n -> Int.to_string n
  | Bool true -> "#t"
  | Bool false -> "#f"
  | If -> "if"
  | Nil -> "nil"
  | Cons (l, r) -> string_of_t (l, g) ^ " :: " ^ string_of_t (r, g)
  | Prim _ -> "<prim>"
  | App (l, r) -> "(" ^ string_of_t (l, g) ^ " " ^ string_of_t (r, g) ^ ")"

let reduce ((v, graph) : t) : unit =
  let add = add_vertex graph in
  let find = Hashtbl.find graph in
  let replace = Hashtbl.replace graph in
  let rec reduce' (v : vertex) : unit =
    match find v with
    | Cons (h, t) -> reduce' h; reduce' t
    | App (f, z) -> begin
        match find f with
        | I ->
            replace v (find z);
            reduce' v
        | Y ->
            let zyz = add (App (z, v)) in
            replace v (find zyz);
            reduce' v
        | Prim prim ->
            let prim_reduced =
              match (prim, find z) with
              | Atom, Int _
              | Atom, Bool _
              | Atom, Nil    -> Some (Bool true)
              | Atom, Cons _ -> Some (Bool false)
              | Car, Cons (h, _) -> Some (find h)
              | Cdr, Cons (_, t) -> Some (find t)
              | _ -> None
            in
            begin
              match prim_reduced with
              | Some v' -> replace v v'; reduce' v
              | None -> reduce' f; reduce' z
            end
        | App (g, y) -> begin
            match find g with
            | K ->
                replace v (find y);
                reduce' v
            | Prim prim ->
                let prim_reduced =
                  match (prim, find y, find z) with
                  | Add, Int n, Int m -> Some (Int (n + m))
                  | Sub, Int n, Int m -> Some (Int (n - m))
                  | Mul, Int n, Int m -> Some (Int (n * m))
                  | Div, Int n, Int m -> Some (Int (n / m))

                  | Lt, Int n, Int m    -> Some (Bool (n < m))
                  | Gt, Int n, Int m    -> Some (Bool (n > m))
                  | Le, Int n, Int m    -> Some (Bool (n <= m))
                  | Ge, Int n, Int m    -> Some (Bool (n >= m))
                  | Eq, Int n, Int m    -> Some (Bool (n = m))
                  | Eq, Bool b, Bool b' -> Some (Bool (b = b'))
                  | Eq, Nil, Nil        -> Some (Bool true)

                  | Cons, _, _        -> Some (Cons (y, z))
                  | _ -> None
                in
                begin
                  match prim_reduced with
                  | Some v' -> replace v v'; reduce' v
                  | None -> reduce' f; reduce' z
                end
            | App (h, x) -> begin
                match find h with
                | S ->
                    let xz = add (App (x, z)) in
                    let yz = add (App (y, z)) in
                    let new_app = add (App (xz, yz)) in
                    replace v (find new_app);
                    reduce' v
                | If ->
                    begin
                      match find x with
                      | Bool b -> replace v (find (if b then y else z)); reduce' v
                      | _ -> reduce' f; reduce' z
                    end
                | _ -> reduce' f; reduce' z
            end
            | _ -> reduce' f; reduce' z
        end
        | _ -> reduce' f; reduce' z
    end
    | _ -> ()
  in
  reduce' v

let rec normalize ((v, g) : t) : t =
  let before = string_of_t (v, g) in
  reduce (v, g);
  let after = string_of_t (v, g) in
  if before = after then (v, g)
  else normalize (v, g)
