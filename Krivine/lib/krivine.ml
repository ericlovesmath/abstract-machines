(* Term language *)
type t =
  | Access of string
  | Grab of string * t
  | Push of t * t
  | If of t * t * t      (* TODO: If is a Prim when Lazy *)
  | Cst of constant

and constant =
  | Nil
  | Cons of t * t  (* TODO: Needs to be value * value to be lazy *)
  | Int of int
  | Bool of bool
  | Prim of Intro.prim

(* Machine components *)
(* TODO: Rename value to closure *)
type value = Cl of t * env
and env = (string * value) list

(* Transition rules *)
let rec evaluate (Cl(t, e)) s =
  match t with
  | Access x -> 
      (match List.assoc_opt x e with
      | Some cl -> evaluate cl s
      | None -> failwith ("Unbound variable: " ^ x))

  | Grab(x, t') ->
      (match s with
      | [] -> Cl(Grab(x, t'), e)
      | cl :: s' -> evaluate (Cl(t', (x, cl) :: e)) s')

  | Push(t1, t2) ->
      evaluate (Cl(t1, e)) (Cl(t2, e) :: s)

  (* Constants *)
  | Cst c ->
      (match c with
      | Nil
      | Cons _
      | Int _
      | Bool _ -> if s = [] then Cl (Cst c, [])
                  else failwith "Applying constant as function"
      | Prim p -> reduce_prim p s)


  | If(c, t, f) ->
      match evaluate (Cl(c, e)) [] with
      | Cl (Cst (Bool true), _) -> evaluate (Cl(t, e)) s
      | Cl (Cst (Bool false), _) -> evaluate (Cl(f, e)) s
      | _ -> failwith "Non-boolean in if condition"

and reduce_prim prim stack =
  let force cl = match evaluate cl [] with
    | Cl(Cst c, []) -> c
    | _ -> failwith "Unexpected forced value is non constant"
  in

  let get_int cl = match force cl with
    | Int n -> n
    | _ -> failwith "Expected integer argument" in

  let get_list cl = match force cl with
    | Cons (hd, tl) -> (Cl (hd, []), Cl (tl, []))
    | Nil -> failwith "Expected non-nil list argument"
    | _ -> failwith "Expected list argument" in

  match prim, stack with
  (* Arithmetic operations *)
  | Add, [a; b] -> Cl(Cst(Int (get_int a + get_int b)), [])
  | Sub, [a; b] -> Cl(Cst(Int (get_int a - get_int b)), [])
  | Mul, [a; b] -> Cl(Cst(Int (get_int a * get_int b)), [])
  | Div, [a; b] -> Cl(Cst(Int (get_int a / get_int b)), [])

  (* Comparisons *)
  | Lt, [a; b] -> Cl(Cst(Bool (get_int a < get_int b)), [])
  | Gt, [a; b] -> Cl(Cst(Bool (get_int a > get_int b)), [])
  | Le, [a; b] -> Cl(Cst(Bool (get_int a <= get_int b)), [])
  | Ge, [a; b] -> Cl(Cst(Bool (get_int a >= get_int b)), [])
  | Eq, [a; b] ->
      (match force a, force b with
      | Int x, Int y -> Cl(Cst(Bool (x = y)), [])
      | Bool x, Bool y -> Cl(Cst(Bool (x = y)), [])
      | _ -> failwith "Type error in equality comparison")

  (* List operations *)
  | Cons, [hd; tl] -> Cl(Cst(Cons (Cst (force hd), Cst (force tl))), [])
  | Car, [lst] -> fst (get_list lst)
  | Cdr, [lst] -> snd (get_list lst)
  | Atom, [lst] ->
      (match force lst with
      | Nil -> Cl(Cst(Bool true), [])
      | Cons _ -> Cl(Cst(Bool false), [])
      | _ -> failwith "Type error in atom check")

  | _ -> failwith "Malformed primitive application"

(* Recursively evaluate to base constant *)
let rec force cl =
  match evaluate cl [] with
  | Cl(Cst c, []) ->
      (match c with
      | Cons(hd, tl) -> Cons (Cst (force (Cl(hd, []))), Cst (force (Cl(tl, []))))
      | _ -> c)
  | Cl(Grab _, _) -> failwith "Cannot force lambda abstraction"
  | Cl(If(c, t, f), env) ->
      (match force (Cl(c, env)) with
      | Bool true -> force (Cl(t, env))
      | Bool false -> force (Cl(f, env))
      | _ -> failwith "Non-boolean in if condition")
  | _ -> failwith "Unexpected closure during forcing"

(* Initial evaluation *)
let eval t = evaluate (Cl(t, [])) []

(* String conversion with proper list formatting *)
let rec string_of_value c =
  match c with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Cons(Cst hd, Cst tl) -> string_of_value hd ^ " :: " ^ string_of_value tl
  | Cons _ -> failwith "Unforced Cons converted to string"
  | Nil -> "nil"
  | Prim _ -> "PRIM"
