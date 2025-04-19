open Sexplib.Std

type t =
  | S | K | Y | C | B | I | U | P (* TODO: rm *)
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | Lam of string * t
  | App of t * t
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

let rec simplify (ast : Uniquify.t) : t =
  match ast with
  | Nil -> Nil
  | Int i -> Int i
  | Bool b -> Bool b
  | Var v -> Var v
  | Prim p -> Prim p

  | App (f, x) -> App (simplify f, simplify x)
  | Lambda ([], _) -> failwith "TODO"
  | Lambda ([arg], body) -> Lam (arg, simplify body)

  (* TODO: Use U/P/K *)
  | Lambda (args, body) ->
      let open Uniquify in
      simplify @@ 
      List.fold_right
        (fun arg e -> Lambda ([arg], e))
        args
        body
