type var = string

type value = List of value list | Int of int | Bool of bool
type instr = NIL | LDC | LD | ATOM | CONS | ADD | SUB | MUL | DIV | Int of int | Var of var

type t = {
  stack : value list;
  env : (var * value) list;
  code : instr list;
  dump : (value list * (var * value) list * instr list) list;
}

let init (sexp : instr Sexp.t) : t =
  { stack = []; env = []; code = Sexp.flatten sexp; dump = [] }

let is_atomic (value : value) =
  match value with
  | Int _
  | Bool _ -> true
  | List _ -> false

let eval_step (state : t) : t =
  let { stack; env; code; _ } = state in
  match (stack, code) with
  | _, [] -> state
  | _, NIL :: code -> { state with code; stack = List [] :: stack }
  | _, LDC :: Int n :: code -> { state with code; stack = Int n :: stack }
  | _, LD :: Var v :: code -> { state with code; stack = List.assoc v env :: stack }
  | v :: stack, ATOM :: code -> { state with code; stack = Bool (is_atomic v) :: stack }
  | v :: List vs :: stack, CONS :: code -> { state with code; stack = List (v :: vs) :: stack }
  | Int n :: Int n' :: stack, ADD :: code -> { state with code; stack = Int (n' + n) :: stack }
  | Int n :: Int n' :: stack, SUB :: code -> { state with code; stack = Int (n' - n) :: stack }
  | Int n :: Int n' :: stack, MUL :: code -> { state with code; stack = Int (n' * n) :: stack }
  | Int n :: Int n' :: stack, DIV :: code -> { state with code; stack = Int (n' / n) :: stack }
  | _ -> failwith "SECD.eval_step: Unimplemented or User Error"


let rec eval (state : t) : value =
  if List.is_empty state.code
    then List.hd state.stack
    else eval (eval_step state)

let rec string_of_value = function
  | List vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b
