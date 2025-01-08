type var = string

type value = List of value list | Int of int | Bool of bool
type instr = NIL | LDC of int | LD of var | ATOM | CONS | ADD | SUB | MUL | DIV

type t = {
  stack : value list;
  env : (var * value) list;
  code : instr list;
  dump : (value list * (var * value) list * instr list) list;
}

let init (code : instr list) : t = { stack = []; env = []; code; dump = [] }

let is_atomic = function
  | Int _
  | Bool _ -> true
  | List _ -> false

let eval_step (state : t) : t =
  let { stack; env; code; dump } = state in
  match (stack, code) with
  | _, [] -> state
  | _, NIL :: code -> { state with code; stack = List [] :: stack }
  | _, LDC n :: code -> { state with code; stack = Int n :: stack }
  | _, LD v :: code -> { state with code; stack = List.assoc v env :: stack }
  | v :: stack, ATOM :: code -> { state with code; stack = Bool (is_atomic v) :: stack }
  | Int n :: Int n' :: stack, ADD :: code -> { state with code; stack = Int (n' + n) :: stack }
  | Int n :: Int n' :: stack, SUB :: code -> { state with code; stack = Int (n' - n) :: stack }
  | Int n :: Int n' :: stack, MUL :: code -> { state with code; stack = Int (n' * n) :: stack }
  | Int n :: Int n' :: stack, DIV :: code -> { state with code; stack = Int (n' / n) :: stack }
  | _ -> failwith "Unimplemented"


let rec eval (state : t) : value =
  if List.is_empty state.code
    then List.hd state.stack
    else eval (eval_step state)

let rec string_of_value = function
  | List vs -> "[" ^ String.concat " " (List.map string_of_value vs) ^ "]"
  | Int n -> string_of_int n
  | Bool b -> string_of_bool b

(* Tests *)
let () =
  let test instrs = instrs |> init |> eval |> string_of_value in
  print_endline @@ test [LDC 5; LDC 10; ADD; LDC 3; DIV];
  print_endline @@ test [LDC 5; LDC 10; SUB; LDC 3; MUL];
