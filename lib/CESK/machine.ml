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
  | CallCC of t

  | Set of string * t
  | Begin of t list

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

type addr = int

type value =
  | Unit
  | Int of int
  | Bool of bool
  | List of value list
  | Closure of t * env
  | Cont of kont
and env = (string * addr) list
and kont = LetKont of string * env * t * kont | Halt

type store = (addr * value) list

(** Note: [Done] contains the env/store just for persistence in REPL *)
type cesk = | Running of t * env * store * kont | Done of env * store * value

let counter = ref 0
let fresh () =
  let a = !counter in incr counter;
  a

let lookup_addr x env =
  try List.assoc x env
  with Not_found -> failwith ("Unbound variable: " ^ x)

let lookup_store a store =
  try List.assoc a store
  with Not_found -> failwith "Address not found in store"

let update_store store a v =
  (a, v) :: List.remove_assoc a store

let eval_atomic (e : t) (env : env) (store : store) : value =
  match e with
  | Error s -> raise (Compiler.RuntimeErr s)
  | Unit -> Unit
  | Nil -> List []
  | Int n -> Int n
  | Bool b -> Bool b
  | Fn _
  | Rec _ -> Closure (e, env)
  | Var v -> lookup_store (lookup_addr v env) store
  | _ -> failwith "eval_atomic: Expected atomic expression"

let apply_kont (k : kont) (v : value) (env : env) (store : store) : cesk =
  match k with
  | Halt -> Done (env, store, v)
  | LetKont (s, env, c, k) ->
      let a = fresh () in
      let env' = (s, a) :: env in
      let store' = update_store store a v in
      Running (c, env', store', k)

let eval_step (c : t) (env : env) (store : store) (k : kont) : cesk =
  let make_int_binop f e e' =
    match (eval_atomic e env store, eval_atomic e' env store) with
    | Int n, Int m -> apply_kont k (f n m) env store
    | _ -> failwith "eval_step: Prim called on non-integer arguments"
  in
  match c with
  | Error _
  | Unit
  | Nil
  | Int _
  | Bool _
  | Fn _
  | Var _ -> apply_kont k (eval_atomic c env store) env store
  | If (cond, t, f) ->
      (match eval_atomic cond env store with
      | Bool true -> Running (t, env, store, k)
      | Bool false -> Running (f, env, store, k)
      | _ -> failwith "eval_step: `If` given non-boolean condition")
  | Let (s, bind, e) -> Running (bind, env, store, LetKont (s, env, e, k))

  | Rec (f, es, body) ->
    let a = fresh () in
    let env' = (f, a) :: env in
    let closure = Closure (Fn (es, body), env') in
    let store' = update_store store a closure in
    apply_kont k closure env store'

  | Call (f, es) ->
      (match eval_atomic f env store, es with
      | Cont k', [f] -> apply_kont k' (eval_atomic f env store) env store
      | Closure (Fn (ss, body), closure_env), _ ->
          let vs = List.map (fun v -> eval_atomic v env store) es in
          let addrs = List.map (fun _ -> fresh ()) vs in
          let env' = List.combine ss addrs @ closure_env in
          let store' = List.fold_left2 update_store store addrs vs in
          Running (body, env', store', k)
      | _ -> failwith "eval_step: Attempted to `Call` non-function value")

  | CallCC f ->
      let closure = eval_atomic f env store in
      (match closure with
      | Closure (Fn ([arg], body), closure_env) ->
          let a = fresh () in
          let env' = (arg, a) :: closure_env in
          let store' = update_store store a (Cont k) in
          Running (body, env', store', k)
      | _ -> failwith "call/cc: argument is not a function")

  | Set (x, e) ->
      let a = lookup_addr x env in
      let v = eval_atomic e env store in
      let store' = update_store store a v in
      apply_kont k Unit env store'

  | Begin [] -> failwith "eval_step: begin has no expressions"
  | Begin [e] -> Running (e, env, store, k)
  | Begin (e :: es) -> Running (e, env, store, LetKont ("_", env, Begin es, k))

  | Add (e, e') -> make_int_binop (fun x y -> Int (x + y)) e e'
  | Sub (e, e') -> make_int_binop (fun x y -> Int (x - y)) e e'
  | Mul (e, e') -> make_int_binop (fun x y -> Int (x * y)) e e'
  | Div (e, e') -> make_int_binop (fun x y -> Int (x / y)) e e'
  | Lt (e, e')  -> make_int_binop (fun x y -> Bool (x < y)) e e'
  | Gt (e, e')  -> make_int_binop (fun x y -> Bool (x > y)) e e'
  | Le (e, e')  -> make_int_binop (fun x y -> Bool (x <= y)) e e'
  | Ge (e, e')  -> make_int_binop (fun x y -> Bool (x >= y)) e e'
  | Eq (e, e')  -> apply_kont k (Bool (eval_atomic e env store = eval_atomic e' env store)) env store
  | Atom e ->
      let b =
        match eval_atomic e env store with
        | Unit | Int _ | Bool _ | List [] -> true
        | Closure _ | List _ | Cont _ -> false
      in
      apply_kont k (Bool b) env store
  | Cons (e, e') ->
    (match (eval_atomic e env store, eval_atomic e' env store) with
      | x, List xs -> apply_kont k (List (x :: xs)) env store
      | _ -> failwith "eval_step: Cons failed")
  | Car e ->
    (match eval_atomic e env store with
      | List (h :: _) -> apply_kont k h env store
      | _ -> failwith "eval_step: Car failed")
  | Cdr e ->
    (match eval_atomic e env store with
      | List (_ :: tl) -> apply_kont k (List tl) env store
      | _ -> failwith "eval_step: Cdr failed")

let eval ((env, store) : env * store) (code : t) : (env * store) * value =
  let rec loop state =
    match state with
    | Running (c, e, s, k) -> loop (eval_step c e s k)
    | Done (e, s, v) -> ((e, s), v)
  in
  loop (Running (code, env, store, Halt))

let rec string_of_value (v : value) : string =
  match v with
  | Int n -> string_of_int n
  | Bool true -> "#t"
  | Bool false -> "#f"
  | List [] -> "nil"
  | Unit -> "#u"
  | List (v :: vs) -> string_of_value v ^ " :: " ^ string_of_value (List vs)
  | Closure _ -> "<closure>"
  | Cont _ -> "<kont>"
