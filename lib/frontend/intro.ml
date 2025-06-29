open Parser
open Sexplib.Std

type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le
  | And | Or | Not | Neq
  | Error of string
  [@@deriving sexp]

type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | List of t list
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | Let of string * string list * t * t
  | LetRec of string * string list * t * t
  | LetStar of (string * string list * t) list * t
  | LetRecStar of (string * string list * t) list * t
  | Call of t * t list
  | Prim of prim
  [@@deriving sexp]

type top =
  | Define of string * string list * t
  | Assert of string option * t
  | Expr of t
  [@@deriving sexp]

let unitP = Unit <$ stringP "#u"
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

(** [let*], but trims comments and whitespace off end *)
let ( let- ) p f = (p <* trimP) >>= f

let parensPT left right p =
  charP left *> strip p <* charP right

(** alphabetic followed by (possibly multiple) ['] or [!] or [?] *)
let varstrP =
  let endP = stringP "!" <|> stringP "?" <|> many (charP '\'') in
  let* keyword = ( @ ) <$> alphaP <*> endP in
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

let errorP =
  parensPT '(' ')' (
    let- _ = stringP "error" in
    let* msg = charP '"' *> many (satisfy (( <> ) '"')) <* charP '"' in
    pure (Prim (Error (implode msg)))
  )

let rec introP st =
  (unitP <|> nilP <|> integerP <|> boolP <|> primP <|> errorP <|> ifP <|> condP
   <|> lambdaP <|> letP <|> letstarP <|> callP <|> listP <|> variableP) st

and ifP st =
  parensPT '(' ')' (
    let- _ = stringP "if" in
    let- c = introP in
    let* t = introP in
    let* f = (trimP *> introP) <|> pure Unit in
    pure (If (c, t, f))
  ) st

and lambdaP st =
  parensPT '(' ')' (
    let- _ = stringP "lambda" in
    let- args = varlistP in
    let* body = introP in
    pure (Lambda (args, body))
  ) st

and letP st =
  parensPT '(' ')' (
    let- letkind = (`LetRec <$ stringP "letrec") <|> (`Let <$ stringP "let") in
    let- args = varlistP in
    let- bind = introP in
    let* body = introP in
    match args with
    | [] -> fail
    | value :: args ->
        match letkind with
        | `Let -> pure (Let (value, args, bind, body))
        | `LetRec -> pure (LetRec (value, args, bind, body))
  ) st

and letstarP st =
  parensPT '(' ')' (
    let- letkind = (`LetRec <$ stringP "letrec*") <|> (`Let <$ stringP "let*") in
    let* val_and_binds =
      many (
        let- args = varlistP in
        let- bind = introP in
        match args with
        | [] -> fail
        | value :: args' -> pure (value, args', bind)
      )
    in
    let* body = introP in
    match letkind with
    | `Let -> pure (LetStar (val_and_binds, body))
    | `LetRec -> pure (LetRecStar (val_and_binds, body))
  ) st

and condP st =
  parensPT '(' ')' (
    let- _ = stringP "cond" in
    let caseP =
      parensPT '(' ')' (
          let- cond = introP in
          let* bind = introP in
          pure (cond, bind)
      )
    in
    let* cases = sepBy1 trimP caseP in
    pure (List.fold_right (fun (c, b) acc -> If (c, b, acc)) cases Unit)
  ) st

and callP st =
  parensPT '(' ')' (
    let* exprs = sepBy1 trimP introP in
    match exprs with
    | [] -> fail
    | [e] -> pure e  (* Make single parens optional *)
    | e :: es -> pure (Call (e, es))
  ) st

(** parses syntactic sugar for lists *)
and listP st =
  ((fun l -> List l) <$> parensPT '[' ']' (sepBy trimP introP)) st

let toplevelP =
  let defineP st =
    parensPT '(' ')' (
      let- _ = stringP "define" in
      let- args = varlistP in
      let* bind = introP in
      match args with
      | [] -> fail
      | f :: args -> pure (Define (f, args, bind))
    ) st
  in

  (* assertP can have an optional string descriptor *)
  let assertP =
    let msgP = charP '"' *> many (satisfy (( <> ) '"')) <* charP '"' in
    parensPT '(' ')' (
      let- _ = stringP "assert" in
      let* expr = introP in
      let* msg = ((fun x -> Some (implode x)) <$> (trimP *> msgP)) <|> pure None in
      pure (Assert (msg, expr))
    )
  in

  let exprP = (fun e -> Expr e) <$> introP in

  (* Reads file and dumps into stream. Technically parses [(use "base.scm"]
     if "base.scm" starts with [)], but I don't care :P *)
  let useP =
    let* msg =
      parensPT '(' ')' (
        stringP "use" *> trimP *> charP '"' *> many (satisfy (( <> ) '"')) <* charP '"'
      )
    in
    let contents = In_channel.(with_open_text (implode msg) input_all) in
    fun st -> Some (Expr Unit, explode contents @ st)
  in

  defineP <|> assertP <|> exprP <|> useP

let parse s =
  let cleanP = trimP <|> (() <$ many emptyP) in
  match (cleanP *> toplevelP <* cleanP) (explode s) with
  | Some (res, rem) -> (res, implode rem)
  | None -> failwith "Intro.parse: Parser failed"
