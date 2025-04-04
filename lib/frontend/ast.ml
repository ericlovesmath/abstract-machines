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

let counter = ref 0

let genvar () =
  let v = "$sugar" ^ string_of_int !counter in
  counter := !counter + 1;
  v

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
  | Prim p -> desugar_prim p

and desugar_prim (p : Intro.prim) =
  match p with
  | And ->
      let (l, r) = (genvar (), genvar ()) in
      Lambda ([ l; r ], If (Var l, If (Var r, Bool true, Bool false), Bool false))
  | Or ->
      let (l, r) = (genvar (), genvar ()) in
      Lambda ([ l; r ], If (Var l, Bool true, If (Var r, Bool true, Bool false)))
  | Not ->
      let x = genvar () in
      Lambda ([ x ], If (Var x, Bool false, Bool true))
  | Neq ->
      let (l, r) = (genvar (), genvar ()) in
      Lambda ([ l; r ], If (Call (Prim Eq, [Var l; Var r]), Bool false, Bool true))
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
  | Le -> Prim Le
