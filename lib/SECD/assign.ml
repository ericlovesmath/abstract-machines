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

let assign_vars (ast : Recursion.t) : t =
  let rec aux (locs : string list list) (ast : Recursion.t) : t =
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
    | Call (f, args) -> Call (aux locs f, List.map (aux locs) args)
    | CallRec (f, args) -> CallRec (aux locs f, List.map (aux locs) args)
  in
  aux [] ast
