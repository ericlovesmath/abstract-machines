open Sexplib.Std

type instr =
  | NIL | LDC | LD
  | Int of int | Bool of bool | List of instr list
  | SEL | JOIN
  | LDF | AP | RTN | DUM | RAP
  | ATOM | CONS | CDR | CAR
  | ADD | SUB | MUL | DIV
  | EQ | GT | LT | GE | LE
  [@@deriving sexp]

type value =
  | List of value list
  | Int of int
  | Bool of bool
  | Func of instr list * value list list
  [@@deriving sexp]

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

(** Advances SECD Machine by one instruction *)
let eval_step (state : t) : t =
  let { stack; env; code; dump } = state in
  let push code stack = { state with code = code; stack = stack } in
  match (stack, env, code, dump) with
  | _, _, [], _ -> state

  | _, _, NIL :: c', _                     -> push c' (List [] :: stack)
  | _, _, LDC :: Int n :: c', _            -> push c' (Int n :: stack)
  | _, _, LDC :: Bool b :: c', _           -> push c' (Bool b :: stack)
  | _, _, LD :: Int y :: Int x :: c', _    -> push c' (List.nth (List.nth env y) x :: stack)

  (* Builtin Unary Operations *)
  | v :: s', _, ATOM :: c', _              -> push c' (Bool (is_atomic v) :: s')
  | v :: List vs :: s', _, CONS :: c', _   -> push c' (List (v :: vs) :: s')
  | List (v :: _) :: s', _, CAR :: c', _   -> push c' (v :: s')
  | List (_ :: vs) :: s', _, CDR :: c', _  -> push c' (List vs :: s')

  (* Builtin Binary Operations *)
  | Int n :: Int n' :: s', _, ADD :: c', _ -> push c' (Int (n + n') :: s')
  | Int n :: Int n' :: s', _, SUB :: c', _ -> push c' (Int (n - n') :: s')
  | Int n :: Int n' :: s', _, MUL :: c', _ -> push c' (Int (n * n') :: s')
  | Int n :: Int n' :: s', _, DIV :: c', _ -> push c' (Int (n / n') :: s')
  | Int n :: Int n' :: s', _, LT :: c', _  -> push c' (Bool (n < n') :: s')
  | Int n :: Int n' :: s', _, GT :: c', _  -> push c' (Bool (n > n') :: s')
  | Int n :: Int n' :: s', _, LE :: c', _  -> push c' (Bool (n <= n') :: s')
  | Int n :: Int n' :: s', _, GE :: c', _  -> push c' (Bool (n >= n') :: s')
  | x :: y :: s', _, EQ :: c', _           -> push c' (Bool (x = y) :: s')

  (* If branching *)
  | Bool true :: s', _, SEL :: List t :: _ :: c', _ ->
      { stack = s'; env; code = t; dump = Code c' :: dump }
  | Bool false :: s', _, SEL :: _ :: List f :: c', _  ->
      { stack = s'; env; code = f; dump = Code c' :: dump }
  | _, _, JOIN :: _, Code c' :: d' ->
      { stack; env; code = c'; dump = d' }

  (* Functions *)
  | _, _, LDF :: List f :: c', _ ->
      { stack = Func (f, env) :: stack; env; code = c'; dump }
  | Func (f, e') :: List v :: s', _, AP :: c', _ ->
      { stack = []; env = v :: e'; code = f; dump = Stack s' :: Env env :: Code c' :: dump }
  | x :: _, _, RTN :: [], Stack s :: Env e :: Code c :: d ->
      { stack = x :: s; env = e; code = c; dump = d }

  (* Recursive Functions *)
  | _, _, DUM :: c', _ -> { stack; env = [] :: env; code = c'; dump }
  | (Func (f, _ :: e') as func) :: List v :: s', _ :: e, RAP :: c', _ ->
      { stack = []; env = (func :: v) :: e'; code = f; dump = Stack s' :: Env e :: Code c' :: dump }

  | _, _, instr :: _, _ ->
      let instr_s = string_of_sexp (sexp_of_instr instr) in
      failwith ("SECD.eval_step: Encountered invalid instr " ^ instr_s)


let rec eval (state : t) : value =
  if List.is_empty state.code
    then List.hd state.stack
    else eval (eval_step state)

let rec string_of_value (v : value) : string =
  match v with
  | List [] -> "nil"
  | List (v :: vs) -> string_of_value v ^ " :: " ^ string_of_value (List vs)
  | Int n -> string_of_int n
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Func _ -> "<func>"
