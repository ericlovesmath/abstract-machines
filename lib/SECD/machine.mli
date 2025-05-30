type t

type instr =
  | NIL | LDC | LD
  | Unit | Int of int | Bool of bool | List of instr list
  | SEL | JOIN
  | LDF | AP | RTN | DUM | RAP
  | ATOM | CONS | CDR | CAR
  | ADD | SUB | MUL | DIV
  | EQ | GT | LT | GE | LE
  [@@deriving sexp]

type value =
  | List of value list
  | Unit
  | Int of int
  | Bool of bool
  | Func of instr list * value list list
  [@@deriving sexp]

(** Creates SECD Machine from SECD code *)
val init : value list -> instr list -> t

(** Evaluates SECD Machine until it terminates, returns value at top of stack *)
val eval : t -> value

(** Converts return values to string for pretty printing purposes *)
val string_of_value : value -> string
