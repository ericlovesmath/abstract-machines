include Compiler.Make (struct
  type value = Machine.value

  let name = "SECD"

  let sexp_of_instrs instrs =
    Sexplib.Sexp.List (List.map Machine.sexp_of_instr instrs)

  let execute program =
    program
    |> Uniquify.uniquify
    |> Debug.trace "uniquify" Frontend.Ast.sexp_of_t
    |> Recursion.tag
    |> Debug.trace "tag lambdarec" Recursion.sexp_of_t
    |> Assign.assign_vars
    |> Debug.trace "assign homes" Assign.sexp_of_t
    |> Flatten.flatten
    |> Debug.trace "flatten" sexp_of_instrs
    |> Machine.init
    |> Machine.eval

  let string_of_value = Machine.string_of_value
end)

(** Runs Sexp directly from file *)
(*
let debugger filename =
  let sexp =
    In_channel.input_all
    |> In_channel.with_open_text filename
    |> Sexplib.Sexp.of_string
  in
  let is =
    match sexp with
    | List xs -> xs
    | _ -> failwith "Expected Sexp"
  in
  is
  |> List.map Machine.instr_of_sexp
  |> Machine.init
  |> Machine.eval
*)
