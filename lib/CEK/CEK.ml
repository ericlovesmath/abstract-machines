include Compiler.Make (struct
  type state = bool
  type value = Machine.value

  let name = "CEK"
  let init = false

  let execute _ program =
    program
    |> Parse.parse
    |> Debug.trace "parse cek" Machine.sexp_of_t
    |> Anf.anf
    |> Debug.trace "anf" Machine.sexp_of_t
    |> Machine.eval
    |> fun v -> (false, v)

  let string_of_value = Machine.string_of_value
end)
