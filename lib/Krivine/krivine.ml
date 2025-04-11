include Compiler.Make (struct
  type value = Machine.const

  let name = "Krivine"

  let execute program =
    program
    |> Parse.parse
    |> Debug.trace "parse krivine" Machine.sexp_of_t
    |> Machine.eval
    |> Debug.trace "final closure" Machine.sexp_of_closure
    |> Machine.force

  let string_of_value = Machine.string_of_const
end)
