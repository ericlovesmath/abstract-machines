open Parser
open Sexplib.Std

type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  | And | Or | Not | Neq
  [@@deriving sexp]

type t =
  | Nil
  | Int of int
  | Bool of bool
  | List of t list
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | Let of string * string list * t * t
  | LetRec of string * string list * t * t
  | Call of t * t list
  | Prim of prim
  [@@deriving sexp]

let nilP = Nil <$ stringP "nil"

(** primitives (reserved keywords) *)
let primP =
  (Prim Add <$ charP '+')
  <|> (Prim Sub <$ charP '-')
  <|> (Prim Mul <$ charP '*')
  <|> (Prim Div <$ charP '/')
  <|> (Prim Atom <$ stringP "atom")
  <|> (Prim Cons <$ stringP "cons")
  <|> (Prim Car <$ stringP "car")
  <|> (Prim Cdr <$ stringP "cdr")
  <|> (Prim Eq <$ charP '=')
  <|> (Prim Lt <$ charP '<')
  <|> (Prim Gt <$ charP '>')
  <|> (Prim Le <$ stringP "<=")
  <|> (Prim Ge <$ stringP ">=")
  <|> (Prim And <$ stringP "and")
  <|> (Prim Or <$ stringP "or")
  <|> (Prim Not <$ stringP "not")
  <|> (Prim Neq <$ stringP "!=")

let trimP = 
  let commentP =
    strip (charP ';' <* many (satisfy (fun c -> c <> '\n')))
  in
  (() <$ many1 commentP) <|> (() <$ spacesP)

let parensPT left right p =
  charP left *> strip p <* charP right

(** alphabetic followed by (possibly multiple) ' *)
let varstrP =
  let* keyword = ( @ ) <$> alphaP <*> many (charP '\'') in
  pure (implode keyword)

let varlistP =
  parensPT '(' ')' (sepBy1 trimP varstrP) <|> ((fun x -> [x]) <$> varstrP)

let variableP = (fun v -> Var v) <$> varstrP

(** numeric, possibly preceeded by +/- *)
let integerP =
  let* num =
    numericP
      <|> (List.cons <$> charP '-' <*> numericP)
      <|> (List.cons <$> charP '+' <*> numericP)
  in
  pure (Int (int_of_string (implode num)))

let boolP = (Bool true <$ stringP "#t") <|> (Bool false <$ stringP "#f")

let rec introP st =
  (nilP <|> integerP <|> boolP <|> primP <|> ifP <|> lambdaP <|> letP <|> callP <|> listP <|> variableP) st

and ifP st =
  begin
    let* _ = charP '(' <* many emptyP <* stringP "if" <* trimP in
    let* c = introP <* trimP in
    let* t = introP <* trimP in
    let* f = introP <* many emptyP <* charP ')' in
    pure (If (c, t, f))
  end st

and lambdaP st =
  begin
    parensPT '(' ')' (
      let* _ = stringP "lambda" <* trimP in
      let* args = varlistP <* trimP in
      let* body = introP in
      pure (Lambda (args, body))
    )
  end st

and letP st =
  parensPT '(' ')' (
    let* letkind = (`LetRec <$ stringP "letrec") <|> (`Let <$ stringP "let") in
    let* args = spacesP *> varlistP <* trimP in
    let* bind = introP <* trimP in
    let* body = introP in
    match args with
    | [] -> fail
    | value :: args ->
        match letkind with
        | `Let -> pure (Let (value, args, bind, body))
        | `LetRec -> pure (LetRec (value, args, bind, body))
  ) st

and callP st =
  begin
    let* exprs = parensPT '(' ')' (sepBy1 trimP introP) in
    match exprs with
    | [] -> fail
    | [e] -> pure e  (* Make single parens optional *)
    | e :: es -> pure (Call (e, es))
  end st

(** parses syntactic sugar for lists *)
and listP st =
  ((fun l -> List l) <$> parensPT '[' ']' (sepBy trimP introP)) st

let parse s =
  match (strip introP) (explode s) with
  | Some (res, []) -> res
  | Some _ -> failwith "Intro.parse: Parsed stream incomplete"
  | None -> failwith "Intro.parse: Parser failed"
