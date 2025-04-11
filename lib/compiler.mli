module type Compiler = sig
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module type Compilable = sig
  type value

  val name : string
  val execute : Frontend.Ast.t -> value
  val string_of_value : value -> string
end

module Make : functor (_ : Compilable) -> Compiler
