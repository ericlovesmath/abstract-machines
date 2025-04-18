include Compiler.Make (struct
  type value = Machine.value

  let name = "SECD"

  let sexp_of_instrs instrs =
    Sexplib.Sexp.List (List.map Machine.sexp_of_instr instrs)

  let execute program =
    program
    |> Assign.assign_vars
    |> Debug.trace "assign homes" Assign.sexp_of_t
    |> Flatten.flatten
    |> Debug.trace "flatten" sexp_of_instrs
    |> Machine.init
    |> Machine.eval

  let string_of_value = Machine.string_of_value
end)
