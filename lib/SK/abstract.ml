open Sexplib.Std

type t =
  | Var of string
  | S | K | Y | C | B | I | U | P | If
  | Int of int
  | Bool of bool
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
[@@deriving sexp]

let rec abstract' (ast : Simplify.t) : t =
  match ast with
  | S -> S
  | K -> K
  | Y -> Y
  | C -> C
  | B -> B
  | I -> I
  | U -> U
  | P -> P
  | If -> If
  | Nil -> Nil
  | Int i -> Int i
  | Bool b -> Bool b
  | Prim p -> Prim p
  | Var v -> Var v
  | App (f, x) -> App (abstract' f, abstract' x)
  | Lam (arg, body) -> (
      let abs arg f = abstract' (Lam (arg, f)) in
      match abstract' body with
      | App (f, g) ->
          (* TODO: rm *)
          let f = Simplify.t_of_sexp (sexp_of_t f) in
          let g = Simplify.t_of_sexp (sexp_of_t g) in
          App (App (S, abs arg f), abs arg g)
      | Var v when arg = v -> I
      | _ -> App (K, abstract' body))

let rec fix (ast : t) : Combinator.t =
  match ast with
  | S -> S
  | K -> K
  | Y -> Y
  | C -> C
  | B -> B
  | I -> I
  | U -> U
  | P -> P
  | If -> If
  | Nil -> Nil
  | Int i -> Int i
  | Bool b -> Bool b
  | App (f, x) -> App (fix f, fix x)
  | Prim p -> Prim p
  | Cons (h, t) -> Cons (fix h, fix t)
  | Var v -> failwith ("abstract.fix: Variable " ^ v ^ "not abstracted")

let abstract e = fix (abstract' e)
