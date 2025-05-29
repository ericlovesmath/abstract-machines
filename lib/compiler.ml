open Frontend

module type Compiler = sig
  type state
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module type Compilable = sig
  type state
  type value

  val name : string
  val execute : state option -> Ast.t -> state option * value
  val string_of_value : value -> string
end

module Make (C : Compilable) : Compiler = struct
  type state = C.state
  type value = C.value

  let name = C.name

  let mb_state = ref None

  let execute program =
    let (mb_state', res) =
      program
      |> Intro.parse
      |> Debug.trace "parse frontend" Intro.sexp_of_t
      |> Ast.desugar
      |> Debug.trace "desugaring" Ast.sexp_of_t
      |> C.execute !mb_state
    in
    mb_state := mb_state';
    res

  let string_of_value = C.string_of_value
end
