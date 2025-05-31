open Sexplib.Std

type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | Lambda of string list * t
  | Y
  | If
  | App of t * t
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

let counter = ref 0

let genvar () =
  let v = "$var." ^ string_of_int !counter in
  counter := !counter + 1;
  v

type var = string
module VarMap = Map.Make(String)

(* Make variables unique in each scope,
   simplifies Lambdas and Calls *)
let uniquify (ast : Frontend.Ast.t) : t =
  let rec aux (m : var VarMap.t) (ast : Frontend.Ast.t) : t =
    match ast with
    | Unit -> Unit
    | Nil -> Nil
    | Int i -> Int i
    | Bool b -> Bool b
    | Prim p -> Prim p
    | Var v -> Var (Option.value (VarMap.find_opt v m) ~default:v)
    | If (c, t, f) -> App (App (App (If, aux m c), aux m t), aux m f)
    | Call (f, args) ->
        List.fold_left (fun acc arg -> App (acc, (aux m arg))) (aux m f) args
    | Lambda (args, body) ->
        let add_arg arg m =
          let arg' = genvar () in
          VarMap.add arg arg' m
        in
        let m' = List.fold_right add_arg args m in
        Lambda (List.map (Fun.flip VarMap.find m') args, aux m' body)
    | LambdaRec (f, args, body) ->
        App (Y, (aux m (Lambda (f :: args, body))))
  in
  aux VarMap.empty ast
