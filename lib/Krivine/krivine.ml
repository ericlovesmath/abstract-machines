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

    (* As our `define` is *always* recursive, we can safetly unbind the excess `v`
       binding that is polluting the environment before evaluating to avoid
       the quadratic explosion of the binding of this recursive variable.
       In the even that `Define` is split into a non-recursive case, handle it here *)
    let state' =
      match program with
      | Frontend.Ast.Expr _ -> state
      | Define (v, _) -> List.remove_assoc v state
    in

    let res =
      expr
      |> Parse.parse
      |> Debug.trace "parse krivine" Machine.sexp_of_t
      |> Machine.eval state'
      |> Debug.trace "final closure" Machine.sexp_of_closure
    in
    let forced_res =
      res
      |> Machine.force
      |> Debug.trace "forced closure" Machine.sexp_of_const
    in

    match program with
    | Expr _ -> (state, forced_res)
    | Define (v, _) -> ((v, res) :: List.remove_assoc v state, forced_res)

  let string_of_value = Machine.string_of_const
end)
