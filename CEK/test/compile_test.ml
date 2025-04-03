open Lib.Compiler

let base =
  ["basics"; "fails"; "higher-ordered"; "lambdas"; "letrec"; "y-combinator"]

module CEKTester = Tester.Make (struct
  module C = CEK
  let files = base
end)

module KrivineTester = Tester.Make (struct
  module C = Krivine
  let files = base @ ["lazy"]
end)

let () =
  CEKTester.test ();
  KrivineTester.test ();
