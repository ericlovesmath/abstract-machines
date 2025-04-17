open Sexplib.Std

type t =
  | Nil
  | Int of int
  | Bool of bool
  | Loc of int * int
  | If of t * t * t
  | Lambda of t
  | LambdaRec of t
  | Call of t * t list
  | CallRec of t * t list  (* TODO *)
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

(** Searches [locs] env to find indicies of [var] *)
let locate (locs : string list list) (var : string) : t =
  let rec aux locs x =
    match locs with
    | [] -> failwith "Assign.locate: Unassigned variable"
    | loc :: locs' ->
        match List.find_index (( = ) var) loc with
        | None -> aux locs' (x + 1)
        | Some y -> Loc (x, y)
  in
  aux locs 0

let assign_vars (ast : Frontend.Ast.t) : t =
  let rec aux (locs : string list list) (recs : string list) (ast : Frontend.Ast.t) : t =
    match ast with
    | Nil -> Nil
    | Int i -> Int i
    | Bool b -> Bool b
    | If (c, t, f) -> If (aux locs recs c, aux locs recs t, aux locs recs f)
    | Var v -> locate locs v
    | Prim f -> Prim f
    | Lambda (args, b) -> Lambda (aux (args :: locs) recs b)
    | LambdaRec (name, args, b) ->
        (* Add function name to environment before processing args *)
        let new_locs = (name :: args) :: locs in
        LambdaRec (aux new_locs (name :: recs) b)
    | Call (f, args) ->
        (* TEMP *)
        match f with
        | Var fname when List.mem fname recs -> 
            CallRec (aux locs recs f, List.map (aux locs recs) args)
        | _ ->
            Call (aux locs recs f, List.map (aux locs recs) args)
  in
  aux [] [] ast
