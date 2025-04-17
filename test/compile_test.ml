open Lib

let base =
  [ "basics"; "fails"; "higher-ordered"; "lambdas"; "y-combinator"; "letrec" ]

module SECDTester = Tester.Make (struct
  module C = SECD
  let files = base
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
