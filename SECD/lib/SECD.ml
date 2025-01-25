type instr =
  | NIL | LDC | LD
  | Int of int | List of instr list
  | SEL | JOIN
  | LDF | AP | RTN
  | ATOM | CONS | CDR | CAR
  | ADD | SUB | MUL | DIV
  | EQ | GT | LT | GE | LE

type value =
  | List of value list
  | Int of int
  | Bool of bool
  | Func of instr list * value list list

type dump =
  | Stack of value list
  | Env of value list list
  | Code of instr list

type t = {
  stack : value list;
  env : value list list;
  code : instr list;
  dump : dump list;
}

let init (instrs : instr list) : t =
  { stack = []; env = []; code = instrs; dump = [] }

let is_atomic (value : value) =
  match value with
  | Int _
  | Bool _ -> true
  | List _
  | Func _ -> false

(* TODO: This is slow as it is not an array, I think *)
let locate (x : int) (y : int) (env : value list list) : value =
  (List.nth (List.nth env y) x)

let eval_step (state : t) : t =
  let { stack; env; code; dump } = state in
  let simpl code stack = { state with code = code; stack = stack } in
  match (stack, env, code, dump) with
  | _, _, [], _ -> state

  | _, _, NIL :: c', _                         -> simpl c' (List [] :: stack)
  | _, _, LDC :: Int n :: c', _                -> simpl c' (Int n :: stack)
  | _, _, LD :: Int y :: Int x :: c', _        -> simpl c' (locate x y env :: stack)

  (* Builtin Unary Operations *)
  | v :: s', _, ATOM :: c', _              -> simpl c' (Bool (is_atomic v) :: s')
  | v :: List vs :: s', _, CONS :: c', _   -> simpl c' (List (v :: vs) :: s')
  | List (v :: _) :: s', _, CAR :: c', _   -> simpl c' (v :: s')
  | List (_ :: vs) :: s', _, CDR :: c', _  -> simpl c' (List vs :: s')

  (* Builtin Binary Operations *)
  | Int n :: Int n' :: s', _, ADD :: c', _ -> simpl c' (Int (n + n') :: s')
  | Int n :: Int n' :: s', _, SUB :: c', _ -> simpl c' (Int (n - n') :: s')
  | Int n :: Int n' :: s', _, MUL :: c', _ -> simpl c' (Int (n * n') :: s')
  | Int n :: Int n' :: s', _, DIV :: c', _ -> simpl c' (Int (n / n') :: s')
  | Int n :: Int n' :: s', _, LT :: c', _  -> simpl c' (Bool (n < n') :: s')
  | Int n :: Int n' :: s', _, GT :: c', _  -> simpl c' (Bool (n > n') :: s')
  | Int n :: Int n' :: s', _, LE :: c', _  -> simpl c' (Bool (n <= n') :: s')
  | Int n :: Int n' :: s', _, GE :: c', _  -> simpl c' (Bool (n >= n') :: s')
  | x :: y :: s', _, EQ :: c', _           -> simpl c' (Bool (x = y) :: s')

  (* If branching *)
  | Bool true :: s', _, SEL :: List t :: _ :: c', _   -> { stack = s'; env; code = t; dump = (Code c') :: dump }
  | Bool false :: s', _, SEL :: _ :: List f :: c', _  -> { stack = s'; env; code = f; dump = (Code c') :: dump }
  | _, _, JOIN :: _, Code c' :: d'                    -> { stack; env; code = c'; dump = d' }

  (* Functions *)
  | _, _, LDF :: List f :: c', _ -> { stack = Func (f, env) :: stack; env; code = c'; dump }
  | Func (f, e') :: List v :: s', _, AP :: c', _ ->
      { stack = []; env = v :: e'; code = f; dump = Stack s' :: Env env :: Code c' :: dump }
  | x :: _, _, RTN :: [], Stack s :: Env e :: Code c :: d ->
      { stack = x :: s; env = e; code = c; dump = d }

  | _ -> failwith "SECD.eval_step: Unimplemented or Compiler Error"


let rec eval (state : t) : value =
  if List.is_empty state.code
    then List.hd state.stack
    else eval (eval_step state)

let rec string_of_value (v : value) : string =
  match v with
  | List [] -> "nil"
  | List vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
  | Func _ -> "<func>"
