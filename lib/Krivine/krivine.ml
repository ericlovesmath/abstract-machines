include Compiler.Make (struct
  type state = Machine.env
  type value = Machine.const

  let name = "Krivine"

  let execute state program =
    let (state', res) =
      program
      |> Parse.parse
      |> Debug.trace "parse krivine" Machine.sexp_of_t
      |> Machine.eval state
    in
    let res' =
      res
      |> Debug.trace "final closure" Machine.sexp_of_closure
      |> Machine.force
    in
    (state', res')

  let string_of_value = Machine.string_of_const
end)
