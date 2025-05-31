module type Compiler = sig
  type state
  type value

  val name : string
  val init : state
  val execute : state -> string -> state * value * string
  val string_of_value : value -> string
end

module type Compilable = sig
  type state
  type value

  val name : string
  val init : state
  val execute : state -> Frontend.Ast.top -> state * value
  val string_of_value : value -> string
end

module Make : functor (_ : Compilable) -> Compiler
