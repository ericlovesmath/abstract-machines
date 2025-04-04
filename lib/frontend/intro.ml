open Parser
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

(** alphabetic followed by (possibly multiple) ' *)
let variableP =
  let keywordP = ( @ ) <$> alphaP <*> many (charP '\'') in
  (fun v -> Var v) <$> (implode <$> keywordP)

(** numeric, possibly preceeded by +/- *)
let integerP =
  let numP =
    numericP
      <|> (List.cons <$> charP '-' <*> numericP)
      <|> (List.cons <$> charP '+' <*> numericP)
  in
  (fun v -> Int v) <$> (int_of_string <$> (implode <$> numP))

let boolP = (Bool true <$ stringP "#t") <|> (Bool false <$ stringP "#f")

let listPT (p : 'a parser) (left : char) (right : char) : 'a list parser =
  charP left *> strip (sepBy1 spacesP p) <* charP right

let rec introP st = (nilP <|> integerP <|> boolP <|> primP <|> variableP <|> callP <|> listP) st

(** lambdas, let bindings, and calls *)
and callP st =
  let string_of_var = function
    | Var v -> v
    | _ -> failwith "Intro.callP: Expected vars in lambda args"
  in
  let call_of_list = function
    | [] -> failwith "unreachable: sepBy1 used in listPT"
    | [e] -> e

    | [Var "lambda"; Var v; b] ->
        Lambda ([v], b)
    | [Var "lambda"; Call (v, vs); b] ->
        Lambda (List.map string_of_var (v :: vs), b)
    | Var "lambda" :: _ ->
        failwith "Intro.parse: `lambda` has incorrect form"

    | [Var "let"; Var v; bind; body] ->
        Call (Lambda ([v], body), [bind])
    | [Var "let"; Call (Var f, args); bind; body] ->
        Call (Lambda ([f], body), [Lambda (List.map string_of_var args, bind)])
    | Var "let" :: _ ->
        failwith "Intro.parse: `let` has incorrect form"

    | [Var "letrec"; Var v; bind; body] ->
        Call (Lambda ([v], body), [LambdaRec (v, [], bind)])
    | [Var "letrec"; Call (Var f, args); bind; body] ->
        Call (Lambda ([f], body), [LambdaRec (f, List.map string_of_var args, bind)])
    | Var "letrec" :: _ ->
        failwith "Intro.parse: `letrec` has incorrect form"

    | [Var "if"; c; t; f] -> If (c, t, f)

    | e :: es -> Call (e, es)
  in
  (call_of_list <$> listPT introP '(' ')') st

(** parses syntactic sugar for lists *)
and listP st =
  let ast_of_list es =
    List.fold_right (fun x acc -> Call (Prim Cons, [x; acc])) es Nil
  in 
  (ast_of_list <$> listPT introP '[' ']') st

let parse s =
  match (strip introP) (explode s) with
  | Some (res, []) -> res
  | Some _ -> failwith "Intro.parse: Parsed stream incomplete"
  | None -> failwith "Intro.parse: Parser failed"
