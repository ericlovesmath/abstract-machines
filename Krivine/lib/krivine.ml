type t =
  | Var of string
  | Lambda of string * t
  | App of t * t
  | Nil
  | Int of int
  | Bool of bool
  | If of t * t * t
  | Prim of Intro.prim

type value =
  | Closure of t * (string * value) list
  | IntVal of int
  | BoolVal of bool
  | ListNil
  | ListCons of value * value
  | PrimOp of Intro.prim

let rec reduce value args =
  match value with
  | IntVal _
  | BoolVal _
  | ListNil
  | ListCons _ ->
      if args = [] then value
      else failwith "Krivine.reduce: Attempted to reduce normal form term"

  | Closure (t, env) ->
      (match t with
      | Nil    -> reduce ListNil args
      | Int n  -> reduce (IntVal n) args
      | Bool b -> reduce (BoolVal b) args
      | Prim p -> reduce (PrimOp p) args
      | Var v  ->
          (match List.assoc_opt v env with
          | Some v' -> reduce v' args
          | None -> failwith ("Krivine.reduce: `" ^ v ^ "` missing binding"))
      | Lambda (x, b) ->
          (match args with
           | [] -> value
           | arg :: args' -> reduce (Closure (b, (x, arg) :: env)) args')
      | App (f, x) ->
          reduce (Closure (f, env)) (Closure (x, env) :: args)
      | If (c, t, f) ->
          (match reduce (Closure (c, env)) [] with
          | BoolVal true -> reduce (Closure (t, env)) args
          | BoolVal false -> reduce (Closure (f, env)) args
          | _ -> failwith "Krivine.reduce: Non-boolean in `if` condition"))

  | PrimOp op ->
      let force closure = reduce closure [] in

      let force_int n =
        match force n with
        | IntVal n -> n
        | _ -> failwith "Krivine.reduce: Expected list in HNF"
      in

      let force_list xs kcons knil =
        match force xs with
        | ListCons (car, cdr) -> kcons car cdr
        | ListNil -> knil ()
        | _ -> failwith "Krivine.reduce: Expected list in HNF"
      in

      (match op, args with
      | Add, [x; y] -> IntVal (force_int x + force_int y)
      | Sub, [x; y] -> IntVal (force_int x - force_int y)
      | Mul, [x; y] -> IntVal (force_int x * force_int y)
      | Div, [x; y] -> IntVal (force_int x / force_int y)

      | Lt,  [x; y] -> BoolVal (force_int x < force_int y)
      | Gt,  [x; y] -> BoolVal (force_int x > force_int y)
      | Le,  [x; y] -> BoolVal (force_int x <= force_int y)
      | Ge,  [x; y] -> BoolVal (force_int x >= force_int y)
      | Eq,  [x; y] ->
          (match force x, force y with
           | IntVal x, IntVal y   -> BoolVal (x = y)
           | BoolVal x, BoolVal y -> BoolVal (x = y)
           | _ -> failwith "Krivine.reduce: `=` expects integers or bools")

      | Cons, [h; t] -> ListCons (h, t)
      | Car, [l] ->
          force_list l
            (fun car _ -> force car)
            (fun () -> failwith "Krivine.reduce: Called `car` of `nil`")
      | Cdr, [l] ->
          force_list l
            (fun _ cdr -> force cdr)
            (fun () -> failwith "Krivine.reduce: Called `cdr` of `nil`")
      | Atom, [l] ->
          force_list l
            (fun _ _ -> BoolVal false)
            (fun () -> BoolVal true)

      | _ -> failwith "Krivine.reduce: Invalid primitive operation usage")

let eval code = reduce (Closure (code, [])) []

let rec force value =
  match value with
  | IntVal _
  | BoolVal _
  | PrimOp _
  | ListNil -> value
  | ListCons (h, t) -> ListCons (force h, force t)
  | Closure (t, env) -> force (reduce (Closure (t, env)) [])

let rec string_of_value value =
  match value with
  | IntVal i -> string_of_int i
  | BoolVal b -> string_of_bool b
  | PrimOp _ -> "Primitive Function"
  | ListNil -> "nil"
  | ListCons (h, t) -> string_of_value h ^ " :: " ^ string_of_value t
  | Closure _ -> "Closure"
