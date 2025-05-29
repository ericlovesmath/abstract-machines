include Compiler.Make (struct
  type state = bool
  type value = Graph.t

  let name = "SK"

  let execute _ program =
    program
    |> Uniquify.uniquify
    |> Debug.trace "SK Uniquify" Uniquify.sexp_of_t
    |> Simplify.simplify
    |> Debug.trace "SK Simplify" Simplify.sexp_of_t
    |> Abstract.abstract
    |> Debug.trace "SK Abstraction" Combinator.sexp_of_t
    |> Optimize.optimize
    |> Debug.trace "SK Optimize" Combinator.sexp_of_t
    |> Combinator.graphify
    |> Debug.trace "Convert to Graph" Graph.sexp_of_t
    |> Graph.reduce
    |> fun v -> (None, v)

  let string_of_value = Graph.string_of_t
end)
