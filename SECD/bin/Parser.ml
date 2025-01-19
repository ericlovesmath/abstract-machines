let explode (s : string) : char list = List.of_seq (String.to_seq s)
let implode (st : char list) : string = String.of_seq (List.to_seq st)

(* TODO: Parse Seq.t instead *)
type 'a parser = char list -> ('a * char list) option

(* TODO: Document these Haskell-like functions *)
let pure (x : 'a) : 'a parser = fun st -> Some (x, st)

let ( >>= ) (p : 'a parser) (f : 'a -> 'b parser) : 'b parser =
  fun st ->
    match p st with
    | None -> None
    | Some (a, st') -> f a st'

let ( <*> ) (pf : ('a -> 'b) parser) (px : 'a parser) : 'b parser =
  let (let*) = Option.bind in
  fun st ->
    let* (f, st') = pf st in
    let* (x, st'') = px st' in
    Some (f x, st'')

let ( <*>| ) (pf : ('a -> 'b) parser) (px : 'a parser lazy_t) : 'b parser =
  let (let*) = Option.bind in
  fun st ->
    let* (f, st') = pf st in
    let* (x, st'') = (Lazy.force px) st' in
    Some (f x, st'')

let ( <$> ) (f : 'a -> 'b) (p : 'a parser) : 'b parser =
  fun st ->
    match p st with
    | None -> None
    | Some (a, st') -> Some (f a, st')

let (<|>) (p : 'a parser) (p' : 'a parser) : 'a parser =
  fun st ->
    match p st with
    | None -> p' st
    | Some res -> Some res

let ( <$ ) f p = (Fun.const <$> pure f) <*> p
let ( $> ) p f = f <$ p
let ( *> ) p q = (Fun.id <$ p) <*> q
let ( <* ) p q = (Fun.const <$> p) <*> q

let rec seq =
  function
    [] -> pure []
  | hd :: tl -> List.cons <$> hd <*> seq tl

let rec many1 p = List.cons <$> p <*>| lazy (many p)
and many p = many1 p <|> pure []

let sepBy1 sep p = List.cons <$> p <*> many (sep *> p)
let sepBy sep p = sepBy1 sep p <|> pure []

let satisfy (pred : char -> bool) : char parser =
  fun st ->
    match st with
    | c :: st' when pred c -> Some (c, st')
    | _ -> None

let charP c = satisfy (( = ) c)
let stringP st = seq (List.map charP st)

(* Tests *)
let _ =
  let run (p : char list parser) (str : string) = 
    match p (explode str) with
    | Some (res, rem) -> Printf.printf "Result: %s, Remain: %s\n" (implode res) (implode rem)
    | None -> Printf.printf "Parser failed\n"
  in
  let _ =
    let p = stringP (explode "hello") <|> stringP (explode "world") in
    run p "hello world";
    run p "world hello";
    run p "worl fails";
  in
  ()
