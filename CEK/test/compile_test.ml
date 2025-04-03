open Lib.Compiler

module CEKTester = Tester.Make (struct
  module C = CEK
  let files = ["basics"; "fails"; "higher-ordered"; "lambdas"; "letrec"; "y-combinator"]
end)

let () =
  CEKTester.test ()
