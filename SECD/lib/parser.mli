val explode : string -> char list
val implode : char list -> string

type 'a parser = char list -> ('a * char list) option

val pure : 'a -> 'a parser
val ( >>= ) : 'a parser -> ('a -> 'b parser) -> 'b parser
val ( <*> ) : ('a -> 'b) parser -> 'a parser -> 'b parser
val ( <$> ) : ('a -> 'b) -> 'a parser -> 'b parser
val ( <|> ) : 'a parser -> 'a parser -> 'a parser

val ( <$ ) : 'a -> 'b parser -> 'a parser
val ( $> ) : 'a parser -> 'b -> 'b parser
val ( <* ) : 'a parser -> 'b parser -> 'a parser
val ( *> ) : 'a parser -> 'b parser -> 'b parser

val seq : 'a parser list -> 'a list parser
val choice : 'a parser list -> 'a parser

val many : 'a parser -> 'a list parser
val many1 : 'a parser -> 'a list parser
val sepBy : 'a parser -> 'b parser -> 'b list parser
val sepBy1 : 'a parser -> 'b parser -> 'b list parser

val satisfy : (char -> bool) -> char parser
val charP : char -> char parser
val stringP : string -> char list parser
val alphaP : char list parser
val numericP : char list parser

val strip : 'a parser -> 'a parser
val spacesP : char list parser
