type t

type instr =
  | NIL | LDC | LD
  | Int of int | List of instr list
  | SEL | JOIN
  | LDF | AP | RTN
  | ATOM | CONS | CDR | CAR
  | ADD | SUB | MUL | DIV
  | EQ | GT | LT | GE | LE

type value =
  | List of value list
  | Int of int
  | Bool of bool
  | Func of instr list * value list list

val init : instr list -> t
val eval : t -> value
val string_of_value : value -> string
