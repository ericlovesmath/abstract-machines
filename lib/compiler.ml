open Frontend

module type Compiler = sig
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module type Compilable = sig
  type value

  val name : string
  val execute : Ast.t -> value
  val string_of_value : value -> string
end

module Make (C : Compilable) : Compiler = struct
  type value = C.value

  let name = C.name

  let execute program =
    program
    |> Intro.parse
    |> Debug.trace "parse frontend" Intro.sexp_of_t
    |> Ast.desugar
    |> Debug.trace "desugaring" Ast.sexp_of_t
    |> C.execute

  let string_of_value = C.string_of_value
end
