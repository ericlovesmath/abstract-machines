module type Compiler = sig
  type state
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module type Compilable = sig
  type state
  type value

  val name : string
  val execute : state option -> Frontend.Ast.t -> state option * value
  val string_of_value : value -> string
end

module Make : functor (_ : Compilable) -> Compiler
