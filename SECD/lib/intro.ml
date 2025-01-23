open Parser

type prim =
  | Atom | Cons | Cdr | Car
  | Add | Sub | Mul | Div
  | Eq | Gt | Lt | Ge | Le

type t =
  | Nil
  | Int of int
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | Call of t * t list
  | Prim of prim

let nilP = Nil <$ stringP "nil"

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

let listPT (p : 'a parser) (left : char) (right : char) : 'a list parser =
  charP left *> strip (sepBy1 spacesP p) <* charP right

let rec introP st = (nilP <|> integerP <|> primP <|> variableP <|> callP <|> listP) st

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

    | [Var "let"; Var v; bind; body] ->
        Call (Lambda ([v], body), [bind])
    | [Var "let"; Call (Var f, args); bind; body] ->
        Call (Lambda ([f], body), [Lambda (List.map string_of_var args, bind)])

    | [Var "if"; c; t; f] -> If (c, t, f)

    | e :: es -> Call (e, es)
  in
  (call_of_list <$> listPT introP '(' ')') st

(* Syntactic Sugar for Lists *)
and listP st =
  let ast_of_list es =
    List.fold_right (fun x acc -> Call (Prim Cons, [x; acc])) es Nil
  in 
  (ast_of_list <$> listPT introP '[' ']') st

let parse s =
  match introP (explode s) with
  | None -> None
  | Some (_, _ :: _) -> None
  | Some (res, []) -> Some res

let rec pp = function
  | Nil -> "nil"
  | Int i -> string_of_int i
  | Var v -> v
  | If (c, t, f) -> "(" ^ pp c ^ " ? " ^ pp t ^ " : " ^ pp f ^ ")"
  | Call (e, es) -> pp e ^ "(" ^ String.concat ", " (List.map pp es) ^ ")"
  | Lambda (args, b) -> "(" ^ String.concat ", " args ^ " -> " ^ pp b ^ ")"
  | Prim Add -> "#+"
  | Prim Sub -> "#-"
  | Prim Mul -> "#*"
  | Prim Div -> "#/"
  | Prim Atom -> "#atom"
  | Prim Cons -> "#cons"
  | Prim Car -> "#car"
  | Prim Cdr -> "#cdr"
  | Prim Eq -> "#="
  | Prim Lt -> "#<"
  | Prim Gt -> "#>"
  | Prim Le -> "#<="
  | Prim Ge -> "#>="
