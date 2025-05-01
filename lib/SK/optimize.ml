let rec optimize (ast : Combinator.t) : Combinator.t =
  match ast with
  | App (App (S, App (K, e)), App (K, e')) -> App (K, App (e, e'))
  | App (App (S, App (K, e)), I) -> e
  | App (App (S, App (K, e)), e') -> App (App (B, e), e')
  (* | App (App (S, e), App (K, e')) -> App (App (C, e), e') *)

  | Cons (h, t) -> Cons (optimize h, optimize t)
  | App (f, x) -> App (optimize f, optimize x)
  | _ -> ast
