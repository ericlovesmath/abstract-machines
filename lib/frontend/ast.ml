open Sexplib.Std

type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  | Error of string
  [@@deriving sexp]

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
  | Prim of prim
  [@@deriving sexp]

type top =
  | Define of string * t
  | Expr of t
  [@@deriving sexp]

let counter = ref 0

let genvar () =
  let v = "$sugar" ^ string_of_int !counter in
  counter := !counter + 1;
  v

let rec desugar (t : Intro.t) : t =
  match t with
  | Unit -> Unit
  | Nil -> Nil
  | Int i -> Int i
  | List es ->
      List.fold_right
        (fun x acc -> Call (Prim Cons, [x; acc]))
        (List.map desugar es) Nil
  | Bool b -> Bool b
  | Var v -> Var v
  | If (c, t, f) -> If (desugar c, desugar t, desugar f)
  | Lambda (args, b) -> Lambda (args, desugar b)
  | Let (v, [], bind, body) ->
      Call (Lambda ([v], desugar body), [desugar bind])
  | Let (f, args, bind, body) ->
      Call (Lambda ([f], desugar body), [Lambda (args, desugar bind)])
  | LetRec (v, [], bind, body) ->
      Call (Lambda ([v], desugar body), [LambdaRec (v, [], desugar bind)])
  | LetRec (f, args, bind, body) ->
      Call (Lambda ([f], desugar body), [LambdaRec (f, args, desugar bind)])
  | LetStar ((f, args, bind) :: rem, body) ->
      desugar (Let (f, args, bind, LetStar (rem, body)))
  | LetStar ([], body) ->
      desugar body
  | LetRecStar ((f, args, bind) :: rem, body) ->
      desugar (LetRec (f, args, bind, LetRecStar (rem, body)))
  | LetRecStar ([], body) ->
      desugar body
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
  | Error s -> Prim (Error s)

let desugar_top (t : Intro.top) : top =
  match t with
  | Expr e -> Expr (desugar e)
  (* TODO: Assert should have better error than 0/0 *)
  | Assert (None, e) -> Expr (If (desugar e, Unit, Prim (Error "assert failed")))
  | Assert (Some s, e) -> Expr (If (desugar e, Unit, Prim (Error s)))
  | Define (v, [], bind) -> Define (v, desugar bind)
  (* TODO: Don't use letrec if not needed *)
  (* TODO: Why does many bindings make this slow? *)
  | Define (v, args, bind) -> Define (v, desugar (LetRec (v, args, bind, Var v)))
