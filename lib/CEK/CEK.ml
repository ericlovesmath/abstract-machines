include Compiler.Make (struct
  type state = Machine.env
  type value = Machine.value

  let name = "CEK"
  let init = []

  let execute state program =
    let expr =
      match program with
      | Frontend.Ast.Expr e -> e
      | Define (_, e) -> e
    in
    let res =
      expr
      |> Parse.parse
      |> Debug.trace "parse cek" Machine.sexp_of_t
      |> Anf.anf
      |> Debug.trace "anf" Machine.sexp_of_t
      |> Machine.eval state
    in
    match program with
    | Expr _ -> (state, res)
    | Define (v, _) -> ((v, res) :: state, res)

  let string_of_value = Machine.string_of_value
end)
