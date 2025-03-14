type t =
  | Nil
  | Int of int
  | Loc of int * int
  | If of t * t * t
  | Lambda of t
  | LambdaRec of t
  | Call of t * t list
  | CallRec of t * t list
  | Prim of Intro.prim

(** Replaces [Var v] with [Loc x y] in the SECD Env stack *)
val assign_vars : Intro.t -> t
