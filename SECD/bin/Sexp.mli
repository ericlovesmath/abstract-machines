type 'a t = List of 'a t list | Atom of 'a

val flatten : 'a t -> 'a list
