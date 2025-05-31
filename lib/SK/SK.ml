include Compiler.Make (struct
  type state = (string * Graph.vertex) list * Graph.graph
  type value = Graph.t

  let name = "SK"
  let init = ([], Hashtbl.create 32)

  let execute state program =
    let expr =
      match program with
      | Frontend.Ast.Expr e -> e
      | Define (_, e) -> e
    in
    let (vertex, graph) =
      expr
      |> Uniquify.uniquify
      |> Debug.trace "SK Uniquify" Uniquify.sexp_of_t
      |> Simplify.simplify
      |> Debug.trace "SK Simplify" Simplify.sexp_of_t
      |> Abstract.abstract
      |> Debug.trace "SK Abstraction" Combinator.sexp_of_t
      |> Optimize.optimize
      |> Debug.trace "SK Optimize" Combinator.sexp_of_t
      |> Combinator.graphify state
      |> Debug.trace "Convert to Graph" Graph.sexp_of_t
      |> Graph.reduce
    in
    match program with
    | Expr _ -> (state, (vertex, graph))
    | Define (v, _) -> (((v, vertex) :: fst state, graph), (vertex, graph))

  let string_of_value = Graph.string_of_t
end)
