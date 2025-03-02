open Intro
open Krivine

let rec parse (e : Intro.t) : Krivine.t =
  match e with
  | Nil -> Nil
  | Int n -> Int n
  | Var v -> Var v
  | If (c, t, f) -> If (parse c, parse t, parse f)

  | Lambda (args, body) -> List.fold_right (fun arg acc -> Lambda (arg, acc)) args (parse body)
  (* | Lambda (args, body) -> Lambda (List.nth args 0, parse body)  (* TODO *) *)

  | LambdaRec _ -> failwith "TODO"

  | Call (f, es) -> List.fold_left (fun acc arg -> App (acc, parse arg)) (parse f) es
  (* | Call (f, es) -> App (parse f, parse (List.nth es 0))  (* TODO *) *)

  | Prim Add -> Add
  | Prim Sub -> Sub
  | Prim Div -> Div
  | Prim Mul -> Mul

  | Prim Lt -> Lt
  | Prim Gt -> failwith "TODO"
  | Prim Le -> failwith "TODO"
  | Prim Ge -> failwith "TODO"
  | Prim Eq -> Eq

  | Prim Atom -> IsEmpty
  | Prim Cons -> Cons
  | Prim Car  -> Head
  | Prim Cdr  -> Tail
