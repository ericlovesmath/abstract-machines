open Machine

let y_combinator =
  Grab
    ( "f",
      Push
        ( Grab ("x", Push (Access "f", Push (Access "x", Access "x"))),
          Grab ("x", Push (Access "f", Push (Access "x", Access "x"))) ) )

let rec parse (e : Frontend.Ast.t) : Machine.t =
  match e with
  | Nil -> Cst Nil
  | Int n -> Cst (Int n)
  | Bool b -> Cst (Bool b)
  | Var v -> Access v
  | Prim p -> Cst (Prim p)
  | If (c, t, f) -> If (parse c, parse t, parse f)
  | Call (f, es) ->
      List.fold_left (fun acc arg -> Push (acc, parse arg)) (parse f) es
  | Lambda (args, body) ->
      List.fold_right (fun arg acc -> Grab (arg, acc)) args (parse body)
  | LambdaRec (f, args, body) ->
      (* TODO: Native `ref` based isn't working for some reason *)
      Push (y_combinator, Grab (f, parse (Lambda (args, body))))
