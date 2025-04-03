module type Compiler = sig
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module type Compilable = sig
  type value

  val name : string
  val execute : Intro.t -> value
  val string_of_value : value -> string
end

module Make(C : Compilable) : Compiler = struct
  type value = C.value

  let parse program =
    match (Intro.parse program) with
    | None -> failwith "Error: Parser fail"
    | Some ast -> ast

  let name = C.name
  let execute program = C.execute (parse program)
  let string_of_value = C.string_of_value
end

module CEK = Make (struct
  type value = CEK.value

  let name = "CEK"
  let execute program = CEK.eval (Anf.anf (Parse_cek.parse program))
  let string_of_value = CEK.string_of_value
end)

module Krivine = Make (struct
  type value = Krivine.constant

  let name = "Krivine"
  let execute program = Krivine.force (Krivine.eval (Parse_krivine.parse program))
  let string_of_value = Krivine.string_of_value
end)
