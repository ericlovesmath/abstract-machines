open Sexplib.Std

type t =
  | Var of string (* TODO: rm *)
  | S | K | Y | C | B | I | U | P
  | If
  | Int of int
  | Bool of bool
  | Nil
  | Cons of t * t
  | Prim of Frontend.Ast.prim
  | App of t * t
  [@@deriving sexp]

(* TODO: This is terrible but it works for now *)
let remove_var (ast : t) : Combinator.t =
  Combinator.t_of_sexp (sexp_of_t ast)

let rec abstract' (ast : Simplify.t) : t =
  match ast with
  (* TODO: rm *)
  | S -> S
  | K -> K
  | Y ->
      (* TODO: TEMP while I fix laziness with Y *)
      let ( $ ) x y = App (x, y) in
      S $ S $ K $ (S $ (K $ (S $ S $ (S $ (S $ S $ K)))) $ K)
  | C -> C
  | B -> B
  | I -> I
  | U -> U
  | P -> P
  | If -> If

  | Nil -> Nil
  | Int i -> Int i
  | Bool b -> Bool b

  | App (f, x) -> App (abstract' f, abstract' x)
  | Prim p -> Prim p

  | Var v -> Var v

  | Lam (arg, body) ->
      let abs arg f = abstract' (Lam (arg, f)) in
      match abstract' body with
      | App (f, g) ->
          let f = Simplify.t_of_sexp (sexp_of_t f) in
          let g = Simplify.t_of_sexp (sexp_of_t g) in
          App (App (S, abs arg f), abs arg g)
      | Var v when arg = v -> I
      | _ -> App (K, abstract' body)

(* TODO: This is terrible but it works for now *)
let abstract e = remove_var (abstract' e)

