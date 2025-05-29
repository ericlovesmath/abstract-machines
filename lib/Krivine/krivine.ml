include Compiler.Make (struct
  type state = Machine.env
  type value = Machine.const

  let name = "Krivine"
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
      |> Debug.trace "parse krivine" Machine.sexp_of_t
      |> Machine.eval state
    in
    let res' =
      res
      |> Debug.trace "final closure" Machine.sexp_of_closure
      |> Machine.force
    in
    match program with
    | Expr _ -> (state, res')
    | Define (v, _) -> ((v, res) :: state, res')

  let string_of_value = Machine.string_of_const
end)
