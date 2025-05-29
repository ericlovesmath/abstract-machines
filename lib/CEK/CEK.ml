include Compiler.Make (struct
  type state = bool
  type value = Machine.value

  let name = "CEK"
  let init = false

  let no_top = function
    | Frontend.Ast.Expr t -> t
    | Define _ -> failwith "define not implemented yet"

  let execute _ program =
    program
    |> no_top
    |> Parse.parse
    |> Debug.trace "parse cek" Machine.sexp_of_t
    |> Anf.anf
    |> Debug.trace "anf" Machine.sexp_of_t
    |> Machine.eval
    |> fun v -> (false, v)

  let string_of_value = Machine.string_of_value
end)
