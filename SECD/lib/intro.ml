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

let nilP = Nil <$ stringP (explode "nil")

let primP =
  (Prim Add <$ charP '+')
  <|> (Prim Sub <$ charP '-')
  <|> (Prim Mul <$ charP '*')
  <|> (Prim Div <$ charP '/')
  <|> (Prim Atom <$ stringP (explode "atom"))
  <|> (Prim Cons <$ stringP (explode "cons"))
  <|> (Prim Car <$ stringP (explode "car"))
  <|> (Prim Cdr <$ stringP (explode "cdr"))
  <|> (Prim Eq <$ charP '=')
  <|> (Prim Lt <$ charP '<')
  <|> (Prim Gt <$ charP '>')
  <|> (Prim Le <$ stringP (explode "<="))
  <|> (Prim Ge <$ stringP (explode ">="))

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

let listPT (p : 'a parser) (left : char) (right : char) : 'a list parser =
  let strip p = many (charP ' ') *> p <* many (charP ' ') in
  let spaces = many1 (charP ' ') in
  charP left *> (strip (sepBy1 spaces p)) <* charP right

let rec introP st = (nilP <|> integerP <|> primP <|> variableP <|> callP <|> listP) st

and callP st =
  let call_of_list = function
    | [] -> failwith "unreachable: sepBy1 used in listPT"
    | [e] -> e
    | [Var "lambda"; Var v; b] -> Lambda ([v], b)
    | [Var "lambda"; Call (v, vs); b] ->
        let string_of_var = function
          | Var v -> v
          | _ -> failwith "Intro.callP: Expected vars in lambda args"
        in
        Lambda (List.map string_of_var (v :: vs), b)  (* TODO: Has to be better solution *)
    | [Var "if"; c; t; f] -> If (c, t, f)  (* TODO: Has to be better solution *)
    | e :: es -> Call (e, es)
  in
  (call_of_list <$> listPT introP '(' ')') st

(* Syntactic Sugar for Lists *)
and listP st =
  let strip p = many (charP ' ') *> p <* many (charP ' ') in
  let brackets p = charP '[' *> strip p <* charP ']' in
  let spaces = many1 (charP ' ') in
  let symlistP = brackets (sepBy spaces (introP)) in
  let rec ast_of_list = function
    | [] -> Nil
    | x :: xs -> Call (Prim Cons, [x; ast_of_list xs])
  in 
  (ast_of_list <$> symlistP) st

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
