open Sexplib.Std

type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | Let of string * t * t
  | If of t * t * t
  | Fn of string list * t
  | Rec of string * string list * t
  | Call of t * t list

  | Atom of t
  | Cons of t * t
  | Cdr of t
  | Car of t

  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t

  | Lt of t * t
  | Gt of t * t
  | Le of t * t
  | Ge of t * t
  | Eq of t * t

  | Error of string
  [@@deriving sexp]

type value =
  | Unit
  | Int of int
  | Bool of bool
  | List of value list
  | Closure of t * env ref
and env = (string * value) list

type kont = LetKont of string * env * t * kont | Halt
type cek = Running of t * env * kont | Done of value

let eval_atomic (e : t) (env : env) : value =
  match e with
  | Error s -> raise (Compiler.RuntimeErr s)
  | Unit -> Unit
  | Nil -> List []
  | Int n -> Int n
  | Bool b -> Bool b
  | Fn _
  | Rec _ -> Closure (e, ref env)
  | Var v ->
      (match List.assoc_opt v env with
      | Some value -> value
      | None -> failwith ("eval_atomic: Failed to find '" ^ v ^ "' in env"))
  | _ -> failwith "eval_atomic: Expected atomic expression"

let apply_kont (k : kont) (v : value) : cek =
  match k with
  | Halt -> Done v
  | LetKont (s, e, c, k) -> Running (c, (s, v) :: e, k)

let eval_step (c : t) (env : env) (k : kont) : cek =
  let make_int_binop f e e' =
    (match (eval_atomic e env, eval_atomic e' env) with
      | Int n, Int m -> apply_kont k (f n m)
      | _ -> failwith "eval_step: Prim called on non-integer arguments")
  in
  match c with
  | Error _
  | Unit
  | Nil
  | Int _
  | Bool _
  | Fn _
  | Var _ -> apply_kont k (eval_atomic c env)
  | If (cond, t, f) ->
      (match eval_atomic cond env with
        | Bool true -> Running (t, env, k)
        | Bool false -> Running (f, env, k)
        | _ -> failwith "eval_step: `If` given non-boolean condition")
  | Let (s, bind, e) -> Running (bind, env, LetKont (s, env, e, k))

  | Rec (f, es, body) ->
      let envref = ref env in
      let closure = Closure (Fn (es, body), envref) in
      envref := (f, closure) :: !envref;
      apply_kont k closure

  | Call (f, es) ->
      (match eval_atomic f env with
        | Closure (Fn (ss, body), envref) ->
            let vs = List.map (fun v -> eval_atomic v env) es in
            Running (body, List.combine ss vs @ !envref, k)
        | _ -> failwith "eval_step: Attempted to `Call` non-function value")

  | Add (e, e') -> make_int_binop (fun x y -> Int (x + y)) e e'
  | Sub (e, e') -> make_int_binop (fun x y -> Int (x - y)) e e'
  | Mul (e, e') -> make_int_binop (fun x y -> Int (x * y)) e e'
  | Div (e, e') -> make_int_binop (fun x y -> Int (x / y)) e e'

  | Lt (e, e')  -> make_int_binop (fun x y -> Bool (x < y)) e e'
  | Gt (e, e')  -> make_int_binop (fun x y -> Bool (x > y)) e e'
  | Le (e, e')  -> make_int_binop (fun x y -> Bool (x <= y)) e e'
  | Ge (e, e')  -> make_int_binop (fun x y -> Bool (x >= y)) e e'
  | Eq (e, e')  -> apply_kont k (Bool (eval_atomic e env = eval_atomic e' env))

  | Atom e ->
      let b =
        match eval_atomic e env with
        | Unit | Int _ | Bool _ | List [] -> true
        | Closure _ | List _ -> false
      in
      apply_kont k (Bool b)
  | Cons (e, e') ->
    (match (eval_atomic e env, eval_atomic e' env) with
      | x, List xs -> apply_kont k (List (x :: xs))
      | _ -> failwith "eval_step: Cons failed")
  | Car e ->
    (match eval_atomic e env with
      | List (h :: _) -> apply_kont k h
      | _ -> failwith "eval_step: Car failed")
  | Cdr e ->
    (match eval_atomic e env with
      | List (_ :: tl) -> apply_kont k (List tl)
      | _ -> failwith "eval_step: Cdr failed")

let eval (state: (string * value) list) (code : t) : value =
  let rec aux state =
    match state with
    | Running (c, e, k) -> aux (eval_step c e k)
    | Done v -> v
  in
  aux (Running (code, state, Halt))

let rec string_of_value (v : value) : string =
  match v with
  | Int n -> string_of_int n
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Unit -> "#u"
  | List [] -> "nil"
  | List (v :: vs) -> string_of_value v ^ " :: " ^ string_of_value (List vs)
  | Closure _ -> "<closure>"
