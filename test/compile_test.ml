open Lib.Compiler

let base =
  ["basics"; "fails"; "higher-ordered"; "lambdas"; "letrec"; "y-combinator"]

module SECDTester = Tester.Make (struct
  module C = SECD
  (* TODO: Missing some passes, SECD not working *)
  let files = ["basics"; "fails"; "lambdas"; "y-combinator"]
end)

module CEKTester = Tester.Make (struct
  module C = CEK
  let files = base
end)

module KrivineTester = Tester.Make (struct
  module C = Krivine
  let files = base @ ["lazy"]
end)

let () =
  SECDTester.test ();
  CEKTester.test ();
  KrivineTester.test ();
