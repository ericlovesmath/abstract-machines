include Compiler.Make (struct
  type value = Combinator.t

  let name = "SKI"

  let execute program =
    program
    |> Uniquify.uniquify
    |> Debug.trace "SKI Uniquify" Uniquify.sexp_of_t
    |> Simplify.simplify
    |> Debug.trace "SKI Simplify" Simplify.sexp_of_t
    |> Abstract.abstract
    |> Debug.trace "SKI Abstraction" Combinator.sexp_of_t
    |> Combinator.eval

  let string_of_value v = Sexplib.Sexp.to_string (Combinator.sexp_of_t v)
end)
