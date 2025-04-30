include Compiler.Make (struct
  type value = Combinator.t

  let name = "SK"

  let execute program =
    program
    |> Uniquify.uniquify
    |> Debug.trace "SK Uniquify" Uniquify.sexp_of_t
    |> Simplify.simplify
    |> Debug.trace "SK Simplify" Simplify.sexp_of_t
    |> Abstract.abstract
    |> Debug.trace "SK Abstraction" Combinator.sexp_of_t
    |> Combinator.eval

  let string_of_value = Combinator.string_of_t
end)
