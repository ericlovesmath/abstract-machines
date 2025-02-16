open Intro
open CEK

let rec parse (e : Intro.t) : CEK.t =
  match e with
  | Nil -> failwith "TODO"
  | Int n -> Int n
  | Var v -> Var v
  | If (c, t, f) -> If (parse c, parse t, parse f)
  | Lambda (args, body) -> Fn (args, parse body)
  | LambdaRec (f, args, body) -> Rec (f, args, parse body)
  | Call (f, es) -> Call (parse f, List.map parse es)
  | Prim Add -> Fn (["!x"; "!y"], Add (Var "!x", Var "!y"))
  | Prim Lt -> Fn (["!x"; "!y"], Lt (Var "!x", Var "!y"))
  | Prim _ -> failwith "TODO"
