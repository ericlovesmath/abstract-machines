open Parser

type 'a t = List of 'a t list | Atom of 'a

let flatten (sexp : 'a t) : 'a list =
  let rec aux (acc : 'a list) (sexp : 'a t) : 'a list =
    match sexp with
      | List [] -> acc
      | List (e :: es) -> aux (aux acc e) (List es)
      | Atom a -> a :: acc
  in
  aux [] sexp


let fail = fun _ -> None
let choice l = List.fold_right (<|>) l fail

let rec pp ppatom  = function
  | Atom a -> ppatom a
  | List es -> "(" ^ String.concat " " (List.map (pp ppatom) es) ^ ")"

let wordP =
  let alphaP =
    let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z') in
    many1 (satisfy is_alpha)
  in
  let numberP =
    let is_numeric c = (c >= '0' && c <= '9') in
    let unsignedP = many1 (satisfy is_numeric) in
    unsignedP
    <|> (List.cons <$> charP '-' <*> unsignedP)
    <|> (List.cons <$> charP '+' <*> unsignedP)
  in
  let mathP =
    let sym c = stringP [c] in
    sym '+' <|> sym '-' <|> sym '/' <|> sym '*'
  in
  alphaP <|> numberP <|> mathP

let atomP = (fun a -> Atom a) <$> (implode <$> wordP)

let listP p =
  let spaces = many1 (charP ' ') in
  let strip p = many (charP ' ') *> p <* many (charP ' ') in
  (fun es -> List es) <$>
    charP '(' *> (strip (sepBy1 spaces p)) <* charP ')'

let rec sexpP st = (atomP <|> listP sexpP) st

let parse s =
  match sexpP (explode s) with
  | None -> None
  | Some (_, _ :: _) -> None
  | Some (res, []) -> Some res
