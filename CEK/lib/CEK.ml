type t =
  | Int of int
  | Bool of bool
  | Add of t * t
  | Lt of t * t
  | Var of string
  | Let of string * t * t
  | If of t * t * t
  | Fn of string list * t
  | Rec of string * string list * t
  | Call of t * t list

type value = Int of int | Bool of bool | Closure of t * env ref
and env = (string * value) list

type kont = LetKont of string * env * t * kont | Halt
type cek = Running of t * env * kont | Done of value

let eval_atomic (e : t) (env : env) : value =
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Fn _ -> Closure (e, ref env)
  | Rec _ -> Closure (e, ref env)
  | Var v -> List.assoc v env
  | Add _ -> failwith "eval_atomic: Expected atomic expression, not Add"
  | Lt _ -> failwith "eval_atomic: Expected atomic expression, not Lt"
  | Let _ -> failwith "eval_atomic: Expected atomic expression, not Let"
  | If _ -> failwith "eval_atomic: Expected atomic expression, not If"
  | Call _ -> failwith "eval_atomic: Expected atomic expression, not Call"

let apply_kont (k : kont) (v : value) : cek =
  match k with
  | Halt -> Done v
  | LetKont (s, e, c, k) -> Running (c, (s, v) :: e, k)

let eval_step (c : t) (env : env) (k : kont) : cek =
  match c with
  | Int _
  | Bool _
  | Fn _
  | Var _ -> apply_kont k (eval_atomic c env)
  | Add (e, e') ->
      begin
        match (eval_atomic e env, eval_atomic e' env) with
        | Int n, Int m -> apply_kont k (Int (n + m))
        | _ -> failwith "eval_step: `Add` called on non-integer arguments"
      end
  | Lt (e, e') ->
      begin
        match (eval_atomic e env, eval_atomic e' env) with
        | Int n, Int m -> apply_kont k (Bool (n < m))
        | _ -> failwith "eval_step: `Lt` called on non-integer arguments"
      end
  | If (cond, t, f) ->
      begin
        match eval_atomic cond env with
        | Bool true -> Running (t, env, k)
        | Bool false -> Running (f, env, k)
        | _ -> failwith "eval_step: `If` given non-boolean condition"
      end
  | Let (s, bind, e) -> Running (bind, env, LetKont (s, env, e, k))

  | Rec (f, es, body) ->
      let envref = ref env in
      let closure = Closure (Fn (es, body), envref) in
      envref := (f, closure) :: !envref;
      apply_kont k closure

  | Call (f, es) ->
      begin
         match eval_atomic f env with
        | Closure (Fn (ss, body), envref) ->
            let vs = List.map (fun v -> eval_atomic v env) es in
            Running (body, List.combine ss vs @ !envref, k)
        | _ -> failwith "eval_step: Attempted to `Call` non-function value"
      end

let eval (code : t) : value =
  let rec aux state =
    match state with
    | Running (c, e, k) -> aux (eval_step c e k)
    | Done v -> v
  in
  aux (Running (code, [], Halt))

let string_of_value (v : value) : string =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure (_, envref) ->
      "Closure in (" ^ String.concat " " (List.map fst !envref) ^ ")"
