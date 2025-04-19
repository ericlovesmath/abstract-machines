include Compiler.Make (struct
  type value = Combinator.t

  let name = "SKI"

  let execute program =
    program
    |> Abstract.abstract
    |> Debug.trace "SKI Abstraction" Combinator.sexp_of_t
    |> Combinator.eval

  let string_of_value v = Sexplib.Sexp.to_string (Combinator.sexp_of_t v)
end)
