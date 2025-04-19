(* TODO: Uniquify and simplify Lambdas to one arg
   TODO: Add pass to abstract AFTER uniquify *)

open Frontend.Ast
open Combinator

let rec abstract (ast : Frontend.Ast.t) : Combinator.t =
  match ast with
  | Nil -> Nil
  | Int i -> Int i
  | Bool b -> Bool b

  | Var _ -> failwith "Var"
  | If _ -> failwith "If"

  | Lambda (args, body) ->
      ignore (args, body); failwith "TODO"
      (* let rec add arg body = *)
      (*   match body with *)
      (*   | App (e, e') -> *)
      (*       let f_e = add arg e in *)
      (*       let f_e' = add arg e' in *)
      (*       App (App (S, f_e), f_e') *)
      (*   | _ -> failwith "TODO" *)
      (* in *)
      (* let body' = abstract body in *)
      (* add "x" body' *)

  | LambdaRec _ -> failwith "LambdaRec"
  | Call (f, args) ->
    List.fold_left (fun acc arg -> App (acc, (abstract arg))) (abstract f) args
  | Prim p -> Prim p
