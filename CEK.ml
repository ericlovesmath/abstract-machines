(* CEK MACHINE CODE *)

type t =
  | Int of int
  | Bool of bool
  | Add of t * t
  | Lt of t * t
  | Var of string
  | Let of string * t * t
  | If of t * t * t
  | Fn of string list * t
  | Call of t * t list

type value = Int of int | Bool of bool | Closure of t * env
and env = (string * value) list

type kont = LetKont of string * env * t * kont | Halt
type cek = Running of t * env * kont | Done of value

let eval_atomic (e : t) (env : env) : value =
  match e with
  | Int n -> Int n
  | Bool b -> Bool b
  | Fn _ -> Closure (e, env)
  | Var v -> List.assoc v env
  | _ -> failwith "eval_atomic: Expected atomic expression"

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
  | Call (f, es) ->
      begin
        match eval_atomic f env with
        | Closure (Fn (ss, body), e') ->
            let vs = List.map (fun v -> eval_atomic v env) es in
            Running (body, List.combine ss vs @ e', k)
        | _ -> failwith "eval_step: Attempted to `Call` non-function value"
      end

let eval (code : t) : value =
  let rec aux state =
    match state with
    | Running (c, e, k) -> aux (eval_step c e k)
    | Done v -> v
  in
  aux (Running (code, [], Halt))

let string_of_val (v : value) : string =
  match v with
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Closure _ -> "Closure"

let is_atomic (e : t) : bool =
  match e with
  | Int _ | Bool _ | Var _ | Fn _ -> true
  | Add _ | Lt _ | Let _ | If _ | Call _ -> false


(* ANF CODE *)

let counter = ref 0

let make_atomic (e : t) (k : t -> t) : t =
  if is_atomic e
    then k e
    else
      let var = "$tmp" ^ string_of_int !counter in
      Let (var, e, k (Var var))

(** TODO: Pull out `atom` so its typed well *)

let anf (e : t) : t =
  let rec aux (e : t) (k : t -> t) : t =
    match e with
    | Int _
    | Bool _
    | Var _ -> e

    | Add (e1, e2) ->
        aux e1 (fun e1' ->
          make_atomic e1' (fun v1 ->
            aux e2 (fun e2' ->
              make_atomic e2' (fun v2 ->
                Add (v1, v2)))))

    | Lt (e1, e2) ->
        aux e1 (fun e1' ->
          make_atomic e1' (fun v1 ->
            aux e2 (fun e2' ->
              make_atomic e2' (fun v2 ->
                Lt (v1, v2)))))

    | Let (x, e1, e2) ->
        aux e1 (fun e1' ->
          Let (x, e1', aux e2 Fun.id))

    | If (cond, then_expr, else_expr) ->
        aux cond (fun cond' ->
          make_atomic cond' (fun v ->
            If (v,
               aux then_expr Fun.id,
               aux else_expr Fun.id)))

    | Fn (params, body) ->
        Fn (params, aux body Fun.id)

    | Call(fn, args) -> failwith "TODO: help me Call ANF"
  in
  aux e Fun.id

(* TESTING CODE *)

let () =
  let test code = print_endline (string_of_val (eval code)) in
  test @@ Lt (Int 3, Int 4);
  test @@ If (Bool true, Int 5, Int 5);
  test @@ Call (Fn (["x"; "y"], Add (Var "x", Var "y")), [Int 2; Int 3]);

  (** A-Normal Form assumed *)
  test @@ Let ("x", Lt (Int 8, Int 4), If (Var "x", Int 5, Int 6));
  test @@ Let ("x", Int 5, Let ("tmp", Add (Var "x", Int 3), Add (Var "tmp", Var "x")));

  test @@ anf @@
    Let ("x", Int 5, Add (Add (Var "x", Int 3), Var "x"));
