type t

type instr =
  | NIL | LDC | LD
  | Int of int | List of instr list
  | SEL | JOIN
  | LDF | AP | RTN | DUM | RAP
  | ATOM | CONS | CDR | CAR
  | ADD | SUB | MUL | DIV
  | EQ | GT | LT | GE | LE

type value =
  | List of value list
  | Int of int
  | Bool of bool
  | Func of instr list * value list list

(** Creates SECD Machine from SECD code *)
val init : instr list -> t

(** Evaluates SECD Machine until it terminates, returns value at top of stack *)
val eval : t -> value

(** Converts return values to string for pretty printing purposes *)
val string_of_value : value -> string
