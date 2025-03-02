type t =
  | Var of string
  | Lambda of string * t
  | App of t * t
  | Int of int
  | Bool of bool
  | If of t * t * t
  | Add | Sub | Mul | Div
  | Eq | Lt | Gt | Le | Ge
  | Nil | Cons | Head | Tail | IsEmpty

type closure =
  | Closure of t * (string * closure) list
  | IntVal of int
  | BoolVal of bool
  | ListNil
  | ListCons of closure * closure
  | PrimOp of prim_op

and prim_op =
  | PAdd | PSub | PMul | PDiv
  | PEq | PLt | PGt | PLe | PGe
  | PCons | PHead | PTail | PIsEmpty

let rec eval clos stack =
  match clos with
  | IntVal _
  | BoolVal _
  | ListNil
  | ListCons _ ->
      if stack = [] then clos else failwith "Value used as function"

  | Closure (t, env) ->
      (match t with
      | Var x -> eval (List.assoc x env) stack
      | Lambda (x, t_body) ->
          (match stack with
           | arg :: stack' -> eval (Closure (t_body, (x, arg) :: env)) stack'
           | [] -> clos)
      | App (t1, t2) ->
          eval (Closure (t1, env)) (Closure (t2, env) :: stack)
      | Int n -> eval (IntVal n) stack
      | Bool b -> eval (BoolVal b) stack
      | Nil -> eval ListNil stack
      | Cons -> eval (PrimOp PCons) stack
      | Head -> eval (PrimOp PHead) stack
      | Tail -> eval (PrimOp PTail) stack
      | IsEmpty -> eval (PrimOp PIsEmpty) stack
      | Add -> eval (PrimOp PAdd) stack
      | Sub -> eval (PrimOp PSub) stack
      | Mul -> eval (PrimOp PMul) stack
      | Div -> eval (PrimOp PDiv) stack
      | Eq -> eval (PrimOp PEq) stack
      | Lt -> eval (PrimOp PLt) stack
      | Gt -> eval (PrimOp PGt) stack
      | Le -> eval (PrimOp PLe) stack
      | Ge -> eval (PrimOp PGe) stack
      | If (cond, t1, t2) ->
          (match eval (Closure (cond, env)) [] with
          | BoolVal true -> eval (Closure (t1, env)) stack
          | BoolVal false -> eval (Closure (t2, env)) stack
          | _ -> failwith "Non-boolean in if condition"))

  | PrimOp op ->
      let force closure = eval closure [] in

      let force_int n =
        match force n with
        | IntVal n -> n
        | _ -> failwith "Expected integer"
      in

      let force_list xs kcons knil =
        match force xs with
        | ListCons (car, cdr) -> kcons car cdr
        | ListNil -> knil ()
        | _ -> failwith "Expected list"
      in

      (match op, stack with
      | PCons, [a; b] -> ListCons (a, b)
      | PHead, [lst] ->
          force_list lst
            (fun car _ -> force car)
            (fun () -> failwith "Head of empty list")
      | PTail, [lst] ->
          force_list lst
            (fun _ cdr -> force cdr)
            (fun () -> failwith "Tail of empty list")
      | PIsEmpty, [lst] ->
          force_list lst
            (fun _ _ -> BoolVal false)
            (fun () -> BoolVal true)
      | PAdd, [a; b] -> IntVal (force_int a + force_int b)
      | PSub, [a; b] -> IntVal (force_int a - force_int b)
      | PMul, [a; b] -> IntVal (force_int a * force_int b)
      | PDiv, [a; b] -> IntVal (force_int a / force_int b)
      | PLt, [a; b] -> BoolVal (force_int a < force_int b)
      | PGt, [a; b] -> BoolVal (force_int a > force_int b)
      | PLe, [a; b] -> BoolVal (force_int a <= force_int b)
      | PGe, [a; b] -> BoolVal (force_int a >= force_int b)
      | PEq, [a; b] ->
          (match force a, force b with
           | IntVal a, IntVal b -> BoolVal (a = b)
           | BoolVal a, BoolVal b -> BoolVal (a = b)
           | _ -> failwith "Eq expects two integers or bools")
      | _ -> failwith @@ "Invalid primitive operation" ^
          string_of_int (List.length stack))

let result term = eval (Closure (term, [])) []

let rec force closure =
  match closure with
  | IntVal _
  | BoolVal _
  | PrimOp _
  | ListNil -> closure
  | ListCons (hd, tl) -> ListCons (force hd, force tl)
  | Closure (t, env) -> force (eval (Closure (t, env)) [])

let rec string_of_value closure =
  match closure with
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | PrimOp _ -> "Primitive Function"
  | ListNil -> "nil"
  | ListCons (hd, tl) -> string_of_value (force hd) ^ " :: " ^ string_of_value (force tl)
  | Closure _ -> "Closure"
