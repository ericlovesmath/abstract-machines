module type Compiler = sig
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module type Compilable = sig
  type value

  val name : string
  val execute : Ast.t -> value
  val string_of_value : value -> string
end

module Make(C : Compilable) : Compiler = struct
  type value = C.value

  let name = C.name

  let execute program =
    program
    |> Intro.parse
    |> Debug.trace "parse frontend" Intro.sexp_of_t
    |> Ast.desugar
    |> Debug.trace "desugaring" Ast.sexp_of_t
    |> C.execute

  let string_of_value = C.string_of_value
end

module SECD = Make (struct
  type value = SECD.value

  let name = "SECD"

  let sexp_of_instrs instrs =
    Sexplib.Sexp.List (List.map SECD.sexp_of_instr instrs)

  let execute program =
    program
    |> Assign.assign_vars
    |> Debug.trace "assign homes" Assign.sexp_of_t
    |> Flatten.flatten
    |> Debug.trace "flatten" sexp_of_instrs
    |> SECD.init
    |> SECD.eval

  let string_of_value = SECD.string_of_value
end)

module CEK = Make (struct
  type value = CEK.value

  let name = "CEK"

  let execute program =
    program
    |> Parse_cek.parse
    |> Debug.trace "parse cek" CEK.sexp_of_t
    |> Anf.anf
    |> Debug.trace "anf" CEK.sexp_of_t
    |> CEK.eval

  let string_of_value = CEK.string_of_value
end)

module Krivine = Make (struct
  type value = Krivine.const

  let name = "Krivine"

  let execute program =
    program
    |> Parse_krivine.parse
    |> Debug.trace "parse krivine" Krivine.sexp_of_t
    |> Krivine.eval
    |> Debug.trace "final closure" Krivine.sexp_of_closure
    |> Krivine.force

  let string_of_value = Krivine.string_of_const
end)
