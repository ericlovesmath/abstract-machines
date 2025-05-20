open Sexplib.Std

type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | LambdaRec of string * string list * t
  | Call of t * t list
  | CallRec of t * t list
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

let rec find_recs (ast : Frontend.Ast.t) : string list =
  match ast with
  | Unit
  | Nil
  | Int _
  | Bool _
  | Prim _
  | Var _ -> []
  | If (c, t, f) -> List.concat_map find_recs [c; t; f]
  | Lambda (_, b) -> find_recs b
  | LambdaRec (name, _, b) -> name :: find_recs b

  (** Recursive functions need to be explicitly called with `RAP` *)
  | Call (Lambda ([f], body), [LambdaRec _ as arg]) ->
      f :: find_recs body @ find_recs arg
  | Call (f, args) -> List.concat_map find_recs (f :: args)

let tag (ast : Frontend.Ast.t) : t =
  let recs = find_recs ast in
  let rec aux (ast : Frontend.Ast.t) : t = 
    match ast with
    | Unit -> Unit
    | Nil -> Nil
    | Int i -> Int i
    | Bool b -> Bool b
    | Prim p -> Prim p
    | Var v -> Var v
    | If (c, t, f) -> If (aux c, aux t, aux f)
    | Lambda (args, b) -> Lambda (args, aux b)
    | LambdaRec (fname, args, b) -> LambdaRec (fname, args, aux b)
    | Call (f, args) ->
        match f with
        | Var fname when List.mem fname recs -> CallRec (aux f, List.map aux args)
        | _ -> Call (aux f, List.map aux args)
  in
  aux ast
