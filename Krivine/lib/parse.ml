open Intro
open Krivine

let y_combinator =
  Lambda
    ( "f",
      App
        ( Lambda ("x", App (Var "f", App (Var "x", Var "x"))),
          Lambda ("x", App (Var "f", App (Var "x", Var "x"))) ) )

let rec parse (e : Intro.t) : Krivine.t =
  match e with
  | Nil -> Nil
  | Int n -> Int n
  | Var v -> Var v
  | If (c, t, f) -> If (parse c, parse t, parse f)

  | Lambda (args, body) -> List.fold_right (fun arg acc -> Lambda (arg, acc)) args (parse body)

  (* TODO: Native binding isn't working for some reason *)
  | LambdaRec (f, args, body) ->
      App (y_combinator, Lambda (f, parse (Lambda (args, body))))

  | Call (f, es) -> List.fold_left (fun acc arg -> App (acc, parse arg)) (parse f) es

  | Prim Add -> Add
  | Prim Sub -> Sub
  | Prim Div -> Div
  | Prim Mul -> Mul

  | Prim Lt -> Lt
  | Prim Gt -> Gt
  | Prim Le -> Le
  | Prim Ge -> Ge
  | Prim Eq -> Eq

  | Prim Atom -> IsEmpty
  | Prim Cons -> Cons
  | Prim Car  -> Head
  | Prim Cdr  -> Tail
