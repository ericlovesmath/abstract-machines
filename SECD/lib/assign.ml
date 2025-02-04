type t =
  | Nil
  | Int of int
  | Loc of int * int
  | If of t * t * t
  | Lambda of t
  | Call of t * t list
  | CallRec of string * t * t list
  | Rec
  | Prim of Intro.prim

(** Searches [locs] env to find indicies of [var] *)
let locate (locs : string list list) (var : string) : t =
  let rec aux locs x =
    match locs with
    | [] -> Rec
    | loc :: locs' ->
        match List.find_index (( = ) var) loc with
        | None -> aux locs' (x + 1)
        | Some y -> Loc (x, y)
  in
  aux locs 0

let assign_vars (ast : Intro.t) : t =
  let rec aux (locs : string list list) (ast : Intro.t) : t =
    match ast with
    | Nil -> Nil
    | Int i -> Int i
    | If (c, t, f) -> If (aux locs c, aux locs t, aux locs f)
    | Var v -> locate locs v
    | Prim f -> Prim f
    | Lambda (args, b) -> Lambda (aux (args :: locs) b)
    | Call (f, args) -> Call (aux locs f, List.map (aux locs) args)
    | LetRec (fname, f, args) -> CallRec (fname, aux locs f, List.map (aux locs) args)
  in
  aux [] ast
