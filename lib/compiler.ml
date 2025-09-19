include Compiler_intf
open Frontend

module Make (C : Compilable) : Compiler = struct
  type state = C.state
  type value = C.value

  let name = C.name
  let init = C.init
  let string_of_value = C.string_of_value

  let execute state program =
    let expr, rem = Intro.parse program in
    let state, value =
      expr
      |> Debug.trace "parse frontend" Intro.sexp_of_top
      |> Ast.desugar_top
      |> Debug.trace "desugaring" Ast.sexp_of_top
      |> C.execute state
    in
    (state, value, rem)
end
