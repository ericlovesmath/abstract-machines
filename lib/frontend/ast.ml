open Sexplib.Std

type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  [@@deriving sexp]

type t =
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | LambdaRec of string * string list * t
  | Call of t * t list
  | Prim of prim
  [@@deriving sexp]

let rec desugar (t : Intro.t) : t =
  match t with
  | Nil -> Nil
  | Int i -> Int i
  | Bool b -> Bool b
  | Var v -> Var v
  | If (c, t, f) -> If (desugar c, desugar t, desugar f)
  | Lambda (args, b) -> Lambda (args, desugar b)
  | LambdaRec (f, args, b) -> LambdaRec (f, args, desugar b)
  | Call (f, args) -> Call (desugar f, List.map desugar args)
  | Prim p ->
      (match p with
      | Atom -> Prim Atom
      | Cons -> Prim Cons
      | Cdr -> Prim Cdr
      | Car -> Prim Car
      | Add -> Prim Add
      | Sub -> Prim Sub
      | Mul -> Prim Mul
      | Div -> Prim Div
      | Eq -> Prim Eq
      | Gt -> Prim Gt
      | Lt -> Prim Lt
      | Ge -> Prim Ge
      | Le -> Prim Le)
