type t
type value
type var = string
type instr = NIL | LDC | LD | ATOM | CONS | ADD | SUB | MUL | DIV | Int of int | Var of var

val init : instr list -> t
val eval : t -> value
val string_of_value : value -> string
