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
  | Prim p -> Prim p
  | If (c, t, f) -> If (parse c, parse t, parse f)
  | Call (f, es) ->
      List.fold_left (fun acc arg -> App (acc, parse arg)) (parse f) es
  | Lambda (args, body) ->
      List.fold_right (fun arg acc -> Lambda (arg, acc)) args (parse body)
  | LambdaRec (f, args, body) ->
      (* TODO: Native `ref` based isn't working for some reason *)
      App (y_combinator, Lambda (f, parse (Lambda (args, body))))
