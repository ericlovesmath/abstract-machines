type t =
  | Nil
  | Int of int
  | Loc of int * int
  | If of t * t * t
  | Lambda of t
  | Call of t * t list
  | Prim of Intro.prim

val assign_vars : Intro.t -> t
