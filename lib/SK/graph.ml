open Sexplib.Std

type vertex = int [@@deriving sexp]

type label =
  | S | K | Y | C | B | I | U | P
  | If
  | Int of int
  | Bool of bool
  | Unit
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
  | Unit -> "#u"
  | Nil -> "nil"
  | Cons (l, r) -> string_of_t (l, g) ^ " :: " ^ string_of_t (r, g)
  | Prim _ -> "<prim>"
  | App _ -> "<lambda>"


(** Convert Graph to DOT file format *)
let dot_of_graph ?(max_len = 20) ?(reachable_only = false) ((root, g) : t) : string =
  let truncate s =
    if String.length s <= max_len then s
    else String.sub s 0 (max_len - 1) ^ "…"
  in
  let escape s =
    let b = Buffer.create (String.length s) in
    String.iter
      (function
        | '"'  -> Buffer.add_string b "\\\""
        | '\\' -> Buffer.add_string b "\\\\"
        | c    -> Buffer.add_char   b c)
      s;
    Buffer.contents b
  in

  (** Collect reachable nodes *)
  let reached : (vertex, unit) Hashtbl.t = Hashtbl.create 256 in
  let rec dfs v =
    if Hashtbl.mem reached v then () else (
      Hashtbl.add reached v ();
      match Hashtbl.find g v with
      | Cons (l, r) | App (l, r) -> dfs l; dfs r
      | _                        -> ()
    )
  in
  if reachable_only
    then dfs root
    else Hashtbl.iter (fun l _ -> Hashtbl.add reached l ()) g;

  (** Emit just reachable nodes *)
  let buf = Buffer.create 4096 in

  let label_of_vertex v =
    match Hashtbl.find g v with
    | S -> "S" | K -> "K" | Y -> "Y" | C -> "C" | B -> "B"
    | I -> "I" | U -> "U" | P -> "P"
    | Int n        -> string_of_int n
    | Bool true    -> "#t"
    | Bool false   -> "#f"
    | If           -> "if"
    | Unit         -> "#u"
    | Nil          -> "nil"
    | Cons _       -> "Cons"
    | App _        -> "App"
    | Prim _       -> "<prim>"
  in

  let add_node v =
    let full  = label_of_vertex v in
    let short = truncate full in
    Printf.bprintf buf
      "  n%d [label=\"%s\", tooltip=\"%s\"%s];\n"
      v (escape short) (escape full)
      (if v = root then ", style=filled, fillcolor=lightblue" else "")
  in
  let add_edge v child =
    Printf.bprintf buf "  n%d -> n%d;\n" v child
  in

  Buffer.add_string buf
    "digraph G {\n\
    \  rankdir=LR;\n\
    \  node [shape=box, fontname=\"monospace\", fontsize=10, \
            margin=\"0.06,0.04\"];\n\
    \  edge [arrowsize=0.6];\n";

  Hashtbl.iter (fun v () -> add_node v) reached;

  Hashtbl.iter
    (fun v () ->
       match Hashtbl.find g v with
       | Cons (l, r) | App (l, r) ->
           add_edge v l; add_edge v r
       | _ -> ())
    reached;

  Buffer.add_string buf "}\n";
  Buffer.contents buf


let log_index = ref 0
(** Only logs graphs to [logs] folder if [Debug.debug] *)
let log_graph (graph : t) : unit =
  if !Debug.debug then
    log_index := !log_index + 1;
    if not (Sys.file_exists "logs") then Sys.mkdir "logs" 0x777;
    let filename = Printf.sprintf "logs/graph%04d.dot" !log_index in
    let dot = dot_of_graph ?reachable_only:(Some true) graph in
    Out_channel.with_open_text
      filename
      (fun ch -> Out_channel.output_string ch dot)


(** Reduce using WHNF (left spine) reduction for laziness *)
let reduce' ((root, g) : t) : unit =
  let find = Hashtbl.find g in
  let replace  = Hashtbl.replace g in
  let add = add_vertex g in

  let rec whnf v =
    match find v with
    | App (f, arg) ->
        (* log_graph (v, g); *)
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

                  | Eq , Cons (h, t) , Cons (h', t') ->
                      let eq_prim = g in
                      let eq_head = add (App (eq_prim, h)) in
                      let eq_heads = add (App (eq_head, h')) in
                      let eq_tail = add (App (eq_prim, t)) in
                      let eq_tails = add (App (eq_tail, t')) in

                      (* Use If to implement AND *)
                      let if_node = add If in
                      let false_node = add (Bool false) in
                      let if_cond = add (App (if_node, eq_heads)) in
                      let if_then = add (App (if_cond, eq_tails)) in
                      let result = add (App (if_then, false_node)) in

                      replace v (find result);
                      Some (find v)

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
    | Cons _ | Bool _ | Int _ | Unit | Nil | Prim _ 
    | S | K | I | B | C | Y | U | P | If -> ()
  in
  whnf root

let reduce (expr : t) : t =
  ignore (log_graph);
  reduce' expr;
  expr
