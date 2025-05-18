include Compiler.Make (struct
  type value = Machine.value

  let name = "CESK"

  let execute program =
    program
    |> Parse.parse
    |> Debug.trace "parse cesk" Machine.sexp_of_t
    |> Anf.anf
    |> Debug.trace "anf" Machine.sexp_of_t
    |> Machine.eval

  let string_of_value = Machine.string_of_value
end)
