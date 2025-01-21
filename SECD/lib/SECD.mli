type t
type var = string
type value = List of value list | Int of int | Bool of bool
type instr = NIL | LDC | LD | ATOM | CONS | CDR | CAR | ADD | SUB | MUL | DIV | Int of int | Var of var

val init : instr list -> t
val eval : t -> value
val string_of_value : value -> string
