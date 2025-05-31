open Frontend

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
  val execute : state -> Ast.top -> state * value
  val string_of_value : value -> string
end

module Make (C : Compilable) : Compiler = struct
  type state = C.state
  type value = C.value

  let name = C.name
  let init = C.init
  let string_of_value = C.string_of_value

  let execute state program =
    let (expr, rem) = Intro.parse program in
    let (state, value) =
      expr
      |> Debug.trace "parse frontend" Intro.sexp_of_top
      |> Ast.desugar_top
      |> Debug.trace "desugaring" Ast.sexp_of_top
      |> C.execute state
    in
    (state, value, rem)
end
