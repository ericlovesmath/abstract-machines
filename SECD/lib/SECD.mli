type t

type value = List of value list | Int of int | Bool of bool
type instr =
  | NIL | LDC | LD | Int of int | List of instr list
  | SEL | JOIN
  | ATOM | CONS | CDR | CAR
  | ADD | SUB | MUL | DIV
  | EQ | GT | LT | GE | LE

val init : instr list -> t
val eval : t -> value
val string_of_value : value -> string
