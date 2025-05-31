include Compiler.Make (struct
  type state = Machine.env * Machine.store
  type value = Machine.value

  let name = "CESK"
  let init = ([], [])

  let execute state program =
    let expr =
      match program with
      | Frontend.Ast.Expr e -> e
      | Define (_, e) -> e
    in
    let ((env, store), value) =
      expr
      |> Parse.parse
      |> Debug.trace "parse cesk" Machine.sexp_of_t
      |> Anf.anf
      |> Debug.trace "anf" Machine.sexp_of_t
      |> Machine.eval state
    in
    match program with
    | Expr _ -> ((env, store), value)
    | Define (v, _) ->
        let addr = Machine.fresh () in
        (((v, addr) :: env, (addr, value) :: store), value)

  let string_of_value = Machine.string_of_value
end)
