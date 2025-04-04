module type Tester = sig
  val test : unit -> unit
end

module type Testable = sig
  module C : Lib.Compiler.Compiler
  val files : string list
end

module Make : functor (_ : Testable) -> Tester
