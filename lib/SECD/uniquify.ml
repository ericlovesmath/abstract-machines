let counter = ref 0

let genvar () =
  let v = "$var." ^ string_of_int !counter in
  counter := !counter + 1;
  v

type var = string
module VarMap = Map.Make(String)

(* Make variables unique in each scope,
   simplifies Lambdas and Calls *)
let uniquify (ast : Frontend.Ast.t) : Frontend.Ast.t =
  let rec aux (m : var VarMap.t) (ast : Frontend.Ast.t) : Frontend.Ast.t =
    match ast with
    | Unit -> Unit
    | Nil -> Nil
    | Int i -> Int i
    | Bool b -> Bool b
    | Prim p -> Prim p
    | Var v -> Var (Option.value (VarMap.find_opt v m) ~default:v)
    | If (c, t, f) -> If (aux m c, aux m t, aux m f)
    | Call (f, args) -> Call (aux m f, List.map (aux m) args)
    | Lambda (args, body) ->
        let add_arg arg m =
          let arg' = genvar () in
          VarMap.add arg arg' m
        in
        let m' = List.fold_right add_arg args m in
        Lambda (List.map (Fun.flip VarMap.find m') args, aux m' body)
    | LambdaRec (f, args, body) ->
        let add_arg arg m =
          let arg' = genvar () in
          VarMap.add arg arg' m
        in
        let m' = List.fold_right add_arg (f :: args) m in
        LambdaRec (VarMap.find f m', List.map (Fun.flip VarMap.find m') args, aux m' body)
  in
  aux VarMap.empty ast
