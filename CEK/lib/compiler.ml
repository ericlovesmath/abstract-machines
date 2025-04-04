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

  let execute program =
    let ast = parse program in
    Debug.debug_print "parse" (Intro.sexp_of_t ast);
    C.execute ast

  let string_of_value = C.string_of_value
end

module CEK = Make (struct
  type value = CEK.value

  let name = "CEK"

  let execute program =
    let ast = Parse_cek.parse program in
    Debug.debug_print "parse cek" (CEK.sexp_of_t ast);
    let anf = Anf.anf ast in
    Debug.debug_print "anf" (CEK.sexp_of_t anf);
    CEK.eval anf

  let string_of_value = CEK.string_of_value
end)

module Krivine = Make (struct
  type value = Krivine.constant

  let name = "Krivine"

  let execute program =
    let ast = Parse_krivine.parse program in
    Debug.debug_print "parse krivine" (Krivine.sexp_of_t ast);
    let closure = Krivine.eval ast in
    Debug.debug_print "final closure" (Krivine.sexp_of_value closure);
    Krivine.force closure

  let string_of_value = Krivine.string_of_value
end)
