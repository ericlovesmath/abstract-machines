open Sexplib.Std

type t =
  | S | K | Y | C | B | I | U | P
  | If
  | Int of int
  | Bool of bool
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
  [@@deriving sexp]

(* TODO: VERY TEMPORARY Combinator evalutor, to be replaced in C *)
(* TODO: Make `If` lazy *)

let rec eval c =
  match c with
  | App (App (App (S, f), g), x) -> eval (App (App (f, x), App (g, x)))
  | App (App (App (C, f), g), x) -> eval (App (App (f, x), g))
  | App (App (App (B, f), g), x) -> eval (App (f, App (g, x)))
  | App (App (U, f), App (App (P, x), y)) -> eval (App (App (f, x), y))
  | App (App (K, x), _) -> eval x
  | App (Y, h) -> eval (App (h, App (Y, h)))
  | App (I, x) -> eval x

  | App (Prim Atom, Int _)
  | App (Prim Atom, Bool _)
  | App (Prim Atom, Nil)    -> Bool true
  | App (Prim Atom, Cons _) -> Bool false

  | App (App (Prim Cons, e), Nil) -> Cons (e, Nil)
  | App (App (Prim Cons, e), Cons (e', e'')) -> Cons (e, Cons (e', e''))
  | App (Prim Car, Cons (e, _)) -> e
  | App (Prim Cdr, Cons (_, e)) -> e

  | App (App (Prim Add, Int i), Int i') -> Int (i + i')
  | App (App (Prim Sub, Int i), Int i') -> Int (i - i')
  | App (App (Prim Mul, Int i), Int i') -> Int (i * i')
  | App (App (Prim Div, Int i), Int i') -> Int (i / i')

  | App (App (Prim Eq, e), e')           -> Bool (e = e')
  | App (App (Prim Gt, Int i), Int i') -> Bool (i >= i')
  | App (App (Prim Lt, Int i), Int i') -> Bool (i <= i')
  | App (App (Prim Ge, Int i), Int i') -> Bool (i > i')
  | App (App (Prim Le, Int i), Int i') -> Bool (i < i')

  | App (App (App (If, Bool true), t), _) -> t
  | App (App (App (If, Bool false), _), f) -> f

  | App (x, y) ->
      let x' = eval x in
      let y' = eval y in
      if x = x' && y = y'
      then App (x, y)
      else eval (App (x', y'))

  | S | K | Y | C | B | I | U | P
  | Int _ | Bool _ | Nil | Cons _
  | Prim _ | If -> c

let rec string_of_t c =
  match c with
  | S -> "S"
  | K -> "K"
  | Y -> "Y"
  | C -> "C"
  | B -> "B"
  | I -> "I"
  | U -> "U"
  | P -> "P"
  | If -> "if"
  | Int i -> string_of_int i
  | Bool true -> "#t"
  | Bool false -> "#f"
  | Nil -> "nil"
  | Cons (l, r) -> string_of_t l ^ " :: " ^ string_of_t r
  | Prim _ -> "prim"
  | App (l, r) -> "(" ^ string_of_t l ^ " " ^ string_of_t r ^ ")"
