open Sexplib.Std

(* TODO: VERY TEMPORARY Combinator evalutor, to be replaced in C *)

type t =
  | S | K | Y | C | B | I | U | P
  | Int of int
  | Bool of bool
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
  [@@deriving sexp]

let rec eval c =
  match c with
  | App (App (App (S, f), g), x) -> eval (App (App (f, x), App (g, x)))
  | App (App (App (C, f), g), x) -> eval (App (App (f, x), g))
  | App (App (App (B, f), g), x) -> eval (App (f, App (g, x)))
  | App (App (U, f), App (App (P, x), y)) -> eval (App (App (f, x), y))
  | App (App (K, x), _) -> eval x
  | App (Y, h) -> eval (App (h, App (Y, h)))
  | App (I, x) -> eval x

  (* TODO: Untested, may not work *)
  | App (Prim Atom, Int _)
  | App (Prim Atom, Bool _)
  | App (Prim Atom, Nil)    -> Bool true
  | App (Prim Atom, Cons _) -> Bool false

  (* TODO: Untested, may not work *)
  | App (App (Prim Cons, e), Nil) -> Cons (e, Nil)
  | App (App (Prim Cons, e), Cons (e', e'')) -> Cons (e, Cons (e', e''))
  | App (Prim Car, Cons (e, _)) -> e
  | App (Prim Cdr, Cons (_, e)) -> e

  | App (App (Prim Add, Int i), Int i') -> Int (i + i')
  | App (App (Prim Sub, Int i), Int i') -> Int (i - i')
  | App (App (Prim Mul, Int i), Int i') -> Int (i * i')
  | App (App (Prim Div, Int i), Int i') -> Int (i / i')

  | App (App (Prim Eq, e), e')           -> Bool (e = e')
  | App (App (Prim Gt, Bool b), Bool b') -> Bool (b >= b')
  | App (App (Prim Lt, Bool b), Bool b') -> Bool (b <= b')
  | App (App (Prim Ge, Bool b), Bool b') -> Bool (b > b')
  | App (App (Prim Le, Bool b), Bool b') -> Bool (b < b')

  | App (x, y) ->
      let x' = eval x in
      let y' = eval y in
      if x = x' && y = y'
      then App (x, y)
      else eval (App (x', y'))

  | S | K | Y | C | B | I | U | P
  | Int _ | Bool _ | Nil | Cons _
  | Prim _ -> c

(*
REPL Tests

let ( $ ) x y = App (x, y) ;;

let succ = S $ (S $ (K $ Plus) $ (K $ Int 1)) $ I ;;
eval (succ $ Int 10) ;;

let eg1 = App (I, I)
let eg2 = App (App (K, K), I)
let eg3 = App (App (App (S, K), S), K)
let eg4 = App (App (Plus, Int 4), Int 3)
let succ = App (App (S, App (App (S, App (K, Plus)), App (K, Int 1))), I)
*)

(*

TODO: Implementing reductions in C instead

let mk x y = App (ref x, ref y)
let graph = mk (mk K S) I
let reduce (graph : graph) : graph ref =
  match graph with
  | App (l, r) ->
    begin
      match (!l, !r) with
      | (I, x) -> r
      | (Y, _) -> ref (App (r, ref (App (l, r))))
      | (l, r) -> 
        begin
          match (!l, r) with
          | (App (K, x), y) -> x
        end
    end
  | _ -> graph
  end

*)
