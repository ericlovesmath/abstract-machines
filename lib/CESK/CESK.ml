include Compiler.Make (struct
  type state = bool
  type value = Machine.value

  let name = "CESK"
  let init = false

  let execute _ program =
    program
    |> Parse.parse
    |> Debug.trace "parse cesk" Machine.sexp_of_t
    |> Anf.anf
    |> Debug.trace "anf" Machine.sexp_of_t
    |> Machine.eval
    |> fun v -> (false, v)

  let string_of_value = Machine.string_of_value
end)
