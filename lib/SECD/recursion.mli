type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | If of t * t * t
  | Lambda of string list * t
  | LambdaRec of string * string list * t
  | Call of t * t list
  | CallRec of t * t list
  | Prim of Frontend.Ast.prim
  [@@deriving sexp]

(** Annotates [Call]'s with [CallRec]'s when calling a recursive function.
  Used to determine usage of [AP] or [RAP] in [machine.ml]
  NOTE: We pass in previously bound strings and assume all are recursive *)
val tag : string list -> Frontend.Ast.t -> t
