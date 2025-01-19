type 'a t = List of 'a t list | Atom of 'a

val flatten : 'a t -> 'a list
val pp : ('a -> string) -> 'a t -> string
val parse : string -> string t option
