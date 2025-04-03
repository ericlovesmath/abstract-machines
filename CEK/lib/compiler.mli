module type Compiler = sig
  type value

  val name : string
  val execute : string -> value
  val string_of_value : value -> string
end

module CEK : Compiler
module Krivine : Compiler
