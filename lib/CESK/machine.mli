type t =
  | Unit
  | Nil
  | Int of int
  | Bool of bool
  | Var of string
  | Let of string * t * t
  | If of t * t * t
  | Fn of string list * t
  | Rec of string * string list * t
  | Call of t * t list
  | CallCC of t

  | Set of string * t
  | Begin of t list

  | Atom of t
  | Cons of t * t
  | Cdr of t
  | Car of t

  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t

  | Lt of t * t
  | Gt of t * t
  | Le of t * t
  | Ge of t * t
  | Eq of t * t

  | Error of string
  [@@deriving sexp]

type value
type addr
type env = (string * addr) list
type store = (addr * value) list

val fresh : unit -> addr

(** Evaluate using CEK Machine, assumes input in ANF *)
val eval : env * store -> t -> (env * store) * value

(** Converts output of [eval] to string *)
val string_of_value : value -> string
