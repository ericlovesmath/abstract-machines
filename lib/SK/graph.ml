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

(** Reduce using WHNF (left spine) reduction for laziness *)
let reduce' ((root, g) : t) : unit =
  let find = Hashtbl.find g in
  let replace  = Hashtbl.replace g in
  let add = add_vertex g in

  let rec whnf v =
    match find v with
    | App (f, arg) ->
        whnf f;
        begin match find f with
        | I ->
            replace v (find arg);
            whnf v
        | Y ->
            let rec_v = add (App (arg, v)) in
            replace v (find rec_v);
            whnf v
        | App (g, y) ->
            begin
            match find g with
            | Prim p ->
              begin
                whnf y; whnf arg;
                let result =
                  match (p, find y, find arg) with
                  | Add, Int n, Int m -> Some (Int (n + m))
                  | Sub, Int n, Int m -> Some (Int (n - m))
                  | Mul, Int n, Int m -> Some (Int (n * m))
                  | Div, Int n, Int m -> Some (Int (n / m))
                  | Lt , Int n, Int m -> Some (Bool (n <  m))
                  | Gt , Int n, Int m -> Some (Bool (n >  m))
                  | Le , Int n, Int m -> Some (Bool (n <= m))
                  | Ge , Int n, Int m -> Some (Bool (n >= m))
                  | Eq , Int n, Int m -> Some (Bool (n =  m))
                  | Eq , Bool a, Bool b -> Some (Bool (a = b))
                  | Eq , Nil    , Nil     -> Some (Bool true)
                  | Eq , Cons _ , Nil
                  | Eq , Nil    , Cons _ -> Some (Bool false)
                  | Cons, _     , _      -> Some (Cons (y, arg))
                  | _                     -> None
                in
                Option.iter (fun lbl -> replace v lbl; whnf v) result
              end
            | K ->
                (* K x y => x *)
                replace v (find y);
                whnf v
            | App (h, x) -> begin
                match find h with
                | S ->
                    (* S x y z => x z (y z) *)
                    let xz   = add (App (x, arg)) in
                    let yz   = add (App (y, arg)) in
                    let v' = add (App (xz, yz)) in
                    replace v (find v');
                    whnf v
                | B ->
                    (* B x y z => x (y z) *)
                    let yz   = add (App (y, arg)) in
                    let v' = add (App (x, yz)) in
                    replace v (find v');
                    whnf v
                | C ->
                    (* C x y z => (x z) y *)
                    let xz   = add (App (x, arg)) in
                    let v' = add (App (xz, y)) in
                    replace v (find v');
                    whnf v
                | If ->
                    (* if c t f => replace with t or f directly *)
                    whnf x;
                    begin match find x with
                    | Bool b ->
                        replace v (find (if b then y else arg));
                        whnf v
                    | _ -> ()
                    end
                | _ -> ()
              end
            | _ -> ()
          end
        | Prim p -> begin
            whnf arg;
            let result =
              match (p, find arg) with
              | Atom, (Int _ | Bool _ | Nil) -> Some (Bool true)
              | Atom, Cons _                 -> Some (Bool false)
              | Car , Cons (h, _)            -> Some (find h)
              | Cdr , Cons (_, t)            -> Some (find t)
              | _                            -> None
            in
            Option.iter (fun lbl -> replace v lbl; whnf v) result
          end
        | _ -> ()
        end
    | Cons _ | Bool _ | Int _ | Nil | Prim _ 
    | S | K | I | B | C | Y | U | P | If -> ()
  in
  whnf root

let reduce (expr : t) : t =
  reduce' expr;
  expr
