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

(* TEMP *)
let rec find_recs (ast : Frontend.Ast.t) : string list =
  match ast with
    | Nil
    | Int _
    | Bool _
    | Prim _
    | Var _ -> []
    | If (c, t, f) -> find_recs c @ find_recs t @ find_recs f
    | Lambda (_, b) -> find_recs b
    | LambdaRec (name, _, b) ->
        name :: find_recs b
    | Call (_, args) -> List.concat_map find_recs args

let assign_vars (ast : Frontend.Ast.t) : t =
  let recs = find_recs ast in
  let rec aux (locs : string list list) (ast : Frontend.Ast.t) : t =
    match ast with
    | Nil -> Nil
    | Int i -> Int i
    | Bool b -> Bool b
    | If (c, t, f) -> If (aux locs c, aux locs t, aux locs f)
    | Var v -> locate locs v
    | Prim f -> Prim f
    | Lambda (args, b) -> Lambda (aux (args :: locs) b)
    | LambdaRec (name, args, b) ->
        (* Add function name to environment before processing args *)
        let new_locs = (name :: args) :: locs in
        LambdaRec (aux new_locs b)
    | Call (f, args) ->
        (* TEMP *)
        match f with
        | Var fname when List.mem fname recs -> 
            CallRec (aux locs f, List.map (aux locs) args)
        | _ ->
            Call (aux locs f, List.map (aux locs) args)
  in
  aux [] ast
