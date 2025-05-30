open Sexplib.Std

type t =
  | Access of string
  | Grab of string * t
  | Push of t * t
    (* TODO: If is a Prim when Lazy *)
  | If of t * t * t
  | Cst of const
  [@@deriving sexp]

and const =
  | Unit
  | Nil
  | Cons of t * t Lazy.t
  | Int of int
  | Bool of bool
  | Prim of Frontend.Ast.prim
  | Closure of closure
  [@@deriving sexp]

and closure = Cl of t * env
  [@@deriving sexp]
and env = (string * closure) list


(* Transition rules *)
let rec evaluate (Cl (t, env)) s =
  match t with
  | Access x -> (
      match List.assoc_opt x env with
      | Some cl -> evaluate cl s
      | None -> failwith ("Unbound variable: " ^ x))

  | Grab (x, t) -> (
      match s with
      | [] -> Cl (Grab (x, t), env)
      | cl :: s' -> evaluate (Cl (t, (x, cl) :: env)) s')

  | Push (t, t') -> evaluate (Cl (t, env)) (Cl (t', env) :: s)

  | Cst (Prim p) -> reduce_prim p s
  | Cst c ->
      if s = [] then Cl (Cst c, [])
      else failwith "Applying constant as function"

  | If (c, t, f) ->
      match evaluate (Cl(c, env)) [] with
      | Cl (Cst (Bool true), _) -> evaluate (Cl (t, env)) s
      | Cl (Cst (Bool false), _) -> evaluate (Cl (f, env)) s
      | _ -> failwith "Non-boolean in if condition"


and reduce_prim prim stack =
  let force cl = match evaluate cl [] with
    | Cl (Cst c, []) -> c
    | _ -> failwith "Unexpected forced value is non constant"
  in

  let force_int cl = match force cl with
    | Int n -> n
    | _ -> failwith "Expected integer argument"
  in

  let force_list cl = match force cl with
    | Cons (hd, tl) -> (Cl (hd, []), Lazy.map (fun t -> Cl (t, [])) tl)
    | Nil -> failwith "Expected non-nil list argument"
    | _ -> failwith "Expected list argument"
  in

  let enclose const = Cl (Cst const, []) in

  match prim, stack with
  (* Arithmetic operations *)
  | Add, [a; b] -> enclose (Int (force_int a + force_int b))
  | Sub, [a; b] -> enclose (Int (force_int a - force_int b))
  | Mul, [a; b] -> enclose (Int (force_int a * force_int b))
  | Div, [a; b] -> enclose (Int (force_int a / force_int b))

  (* Comparisons *)
  | Lt, [a; b] -> enclose (Bool (force_int a < force_int b))
  | Gt, [a; b] -> enclose (Bool (force_int a > force_int b))
  | Le, [a; b] -> enclose (Bool (force_int a <= force_int b))
  | Ge, [a; b] -> enclose (Bool (force_int a >= force_int b))
  | Eq, [a; b] ->
      (match force a, force b with
      | Int x, Int y   -> enclose (Bool (x = y))
      | Bool x, Bool y -> enclose (Bool (x = y))
      | Nil, Nil       -> enclose (Bool true)
      | Cons _, Nil    -> enclose (Bool false)
      | Nil, Cons _    -> enclose (Bool false)
      | Cons (l, r), Cons (l', r') -> enclose (Bool (l = l' && r = r'))
      | _ -> failwith "Type error in equality comparison")

  (* List operations *)
  | Car, [lst] -> fst (force_list lst)
  | Cdr, [lst] -> Lazy.force (snd (force_list lst))
  | Cons, [hd; tl] ->
      let tl' = Lazy.map (fun tl -> Cst (force tl)) (lazy tl) in
      enclose (Cons (Cst (force hd), tl'))
  | Atom, [lst] -> (
      match force lst with
      | Nil -> enclose (Bool true)
      | Cons _ -> enclose (Bool false)
      | _ -> failwith "Type error in atom check")

  | _ -> failwith "Malformed primitive application"


(* Recursively evaluate to base constant *)
let rec force cl =
  match evaluate cl [] with
  | Cl (Cst c, []) -> (
      match c with
      | Cons (hd, tl) ->
          let reduce t = Cst (force (Cl (t, []))) in
          Cons (reduce hd, Lazy.map reduce tl)
      | _ -> c)
  | Cl (If (c, t, f), env) -> (
      match force (Cl (c, env)) with
      | Bool true -> force (Cl (t, env))
      | Bool false -> force (Cl (f, env))
      | _ -> failwith "Non-boolean in if condition")
  | closure -> Closure closure

let eval env t = evaluate (Cl (t, env)) []

let rec string_of_const c =
  match c with
  | Int i -> string_of_int i
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Unit -> "#u"
  | Nil -> "nil"
  | Prim _ -> "<primitive>"
  | Cons (hd, tl) ->
      (match hd, Lazy.force tl with
      | Cst hd, Cst tl -> string_of_const hd ^ " :: " ^ string_of_const tl
      | _ -> failwith "Unforced Cons converted to string")
  | Closure _ -> "<lambda>"
