open Parser

type prim = Add | Sub | Mul | Div

type t =
  | Nil
  | Int of int
  | Var of string
  | Call of t * t list
  | Prim of prim

let nilP = Nil <$ stringP (explode "nil")

let primP =
  (Prim Add <$ charP '+')
  <|> (Prim Sub <$ charP '-')
  <|> (Prim Mul <$ charP '*')
  <|> (Prim Div <$ charP '/')

(** alphabetic followed by (possibly multiple) ' *)
let variableP =
  let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
  let alphaP = many1 (satisfy is_alpha) in
  let primeP = many (charP '\'') in
  let keywordP = ( @ ) <$> alphaP <*> primeP in
  (fun v -> Var v) <$> (implode <$> keywordP)

(** numeric, possibly preceeded by +/- *)
let integerP =
  let is_numeric c = (c >= '0' && c <= '9') in
  let unsignedP = many1 (satisfy is_numeric) in
  let numP =
    unsignedP
      <|> (List.cons <$> charP '-' <*> unsignedP)
      <|> (List.cons <$> charP '+' <*> unsignedP)
  in
  (fun v -> Int v) <$> (int_of_string <$> (implode <$> numP))

let atomP = nilP <|> integerP <|> primP <|> variableP

let listPT p =
  let strip p = many (charP ' ') *> p <* many (charP ' ') in
  let spaces = many1 (charP ' ') in
  charP '(' *> (strip (sepBy1 spaces p)) <* charP ')'

let rec callP st =
  let call_of_list = function
    | [] -> failwith "unreachable: sepBy1 used in listPT"
    | [e] -> e
    | e :: es -> Call (e, es)
  in
  (call_of_list <$> listPT (atomP <|> callP)) st

let parse s =
  match callP (explode s) with
  | None -> None
  | Some (_, _ :: _) -> None
  | Some (res, []) -> Some res

let rec pp = function
  | Nil -> "nil"
  | Int i -> string_of_int i
  | Var v -> v
  | Call (e, es) -> pp e ^ "(" ^ String.concat ", " (List.map pp es) ^ ")"
  | Prim Add -> "#+"
  | Prim Sub -> "#-"
  | Prim Mul -> "#*"
  | Prim Div -> "#/"
