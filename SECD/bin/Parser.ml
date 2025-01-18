let explode (s : string) : char list = List.of_seq (String.to_seq s)
let implode (st : char list) : string = String.of_seq (List.to_seq st)

(* TODO: Parse Seq.t instead *)
(* TODO: Document these Haskell-like functions *)
type 'a parser = char list -> ('a * char list) option

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

(* Combinators *)
let char (c : char) : char parser =
  fun st ->
    match st with
    | ch :: st' when c = ch -> Some (ch, st')
    | _ -> None

let rec string (s : char list) : char list parser =
  match s with
  | [] -> pure []
  | c :: s' -> (fun c y -> c :: y) <$> char c <*> string s'

(* Tests *)
let () =
  let str = explode "hello world 1" in
  let p = string (explode "hello") in
  match p str with
  | Some (result, rest) -> Printf.printf "Parsed: %s, Remaining: %s\n" (implode result) (implode rest)
  | None -> Printf.printf "Parse failed\n"
