type var = string

type value = List of value list | Int of int | Bool of bool
type instr = NIL | LDC | LD | ATOM | CONS | CDR | CAR | ADD | SUB | MUL | DIV | Int of int | Var of var

type t = {
  stack : value list;
  env : (var * value) list;
  code : instr list;
  dump : (value list * (var * value) list * instr list) list;
}

let init (instrs : instr list) : t =
  { stack = []; env = []; code = instrs; dump = [] }

let is_atomic (value : value) =
  match value with
  | Int _
  | Bool _ -> true
  | List _ -> false

let eval_step (state : t) : t =
  let { stack; env; code; _ } = state in
  let simpl code stack = { state with code = code; stack = stack } in
  match (stack, code) with
  | _, [] -> state
  | _, NIL :: code'                         -> simpl code' (List [] :: stack)
  | _, LDC :: Int n :: code'                -> simpl code' (Int n :: stack)
  | _, LD :: Var v :: code'                 -> simpl code' (List.assoc v env :: stack)
  | v :: stack', ATOM :: code'              -> simpl code' (Bool (is_atomic v) :: stack')
  | v :: List vs :: stack', CONS :: code'   -> simpl code' (List (v :: vs) :: stack')
  | List (v :: vs) :: stack', CAR :: code'  -> simpl code' (v :: stack')
  | List (v :: vs) :: stack', CDR :: code'  -> simpl code' (List vs :: stack')
  | Int n :: Int n' :: stack', ADD :: code' -> simpl code' (Int (n + n') :: stack')
  | Int n :: Int n' :: stack', SUB :: code' -> simpl code' (Int (n - n') :: stack')
  | Int n :: Int n' :: stack', MUL :: code' -> simpl code' (Int (n * n') :: stack')
  | Int n :: Int n' :: stack', DIV :: code' -> simpl code' (Int (n / n') :: stack')
  | _ -> failwith "SECD.eval_step: Unimplemented or User Error"


let rec eval (state : t) : value =
  if List.is_empty state.code
    then List.hd state.stack
    else eval (eval_step state)

let rec string_of_value = function
  | List [] -> "nil"
  | List vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
