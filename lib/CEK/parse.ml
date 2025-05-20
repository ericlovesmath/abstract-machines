open Machine

let counter = ref 0

let genvar () =
  let v = "$tmp" ^ string_of_int !counter in
  counter := !counter + 1;
  v

let make_binop (cons : t -> t -> t) : t =
    let x = genvar () in
    let y = genvar () in
    Fn ([x; y], cons (Var x) (Var y))

let rec parse (e : Frontend.Ast.t) : Machine.t =
  match e with
  | Unit -> Unit
  | Nil -> Nil
  | Int n -> Int n
  | Bool b -> Bool b
  | Var v -> Var v
  | If (c, t, f) -> If (parse c, parse t, parse f)
  | Lambda (args, body) -> Fn (args, parse body)
  | LambdaRec (f, args, body) -> Rec (f, args, parse body)

  | Call (Prim Add, [e; e']) -> Add (parse e, parse e')
  | Call (Prim Sub, [e; e']) -> Sub (parse e, parse e')
  | Call (Prim Div, [e; e']) -> Div (parse e, parse e')
  | Call (Prim Mul, [e; e']) -> Mul (parse e, parse e')

  | Call (Prim Lt, [e; e']) -> Lt (parse e, parse e')
  | Call (Prim Gt, [e; e']) -> Gt (parse e, parse e')
  | Call (Prim Le, [e; e']) -> Le (parse e, parse e')
  | Call (Prim Ge, [e; e']) -> Ge (parse e, parse e')
  | Call (Prim Eq, [e; e']) -> Eq (parse e, parse e')

  | Call (Prim Atom, [e])     -> Atom (parse e)
  | Call (Prim Cons, [e; e']) -> Cons (parse e, parse e')
  | Call (Prim Car, [e])      -> Car (parse e)
  | Call (Prim Cdr, [e])      -> Cdr (parse e)

  | Call (Prim _, _) -> failwith "Parse.parse: Unexpected call of Prim"
  | Call (f, es) -> Call (parse f, List.map parse es)

  | Prim Add -> make_binop (fun x y -> Add (x, y))
  | Prim Sub -> make_binop (fun x y -> Sub (x, y))
  | Prim Div -> make_binop (fun x y -> Div (x, y))
  | Prim Mul -> make_binop (fun x y -> Mul (x, y))

  | Prim Lt -> make_binop (fun x y -> Lt (x, y))
  | Prim Gt -> make_binop (fun x y -> Gt (x, y))
  | Prim Le -> make_binop (fun x y -> Le (x, y))
  | Prim Ge -> make_binop (fun x y -> Ge (x, y))
  | Prim Eq -> make_binop (fun x y -> Eq (x, y))

  | Prim Atom -> let x = genvar () in Fn ([x], Atom (Var x))
  | Prim Cons -> make_binop (fun x y -> Cons (x, y))
  | Prim Car  -> let x = genvar () in Fn ([x], Car (Var x))
  | Prim Cdr  -> let x = genvar () in Fn ([x], Cdr (Var x))
