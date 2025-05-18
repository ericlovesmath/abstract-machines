open Lib

let base =
  [ "basics"; "fails"; "higher-ordered"; "lambdas"; "y-combinator"; "letrec"; "fancy" ]

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

module SKTester = Tester.Make (struct
  module C = SK
  let files = base
end)

module CESKTester = Tester.Make (struct
  module C = CESK
  let files = base
end)

let () =
  SECDTester.test ();
  CEKTester.test ();
  KrivineTester.test ();
  SKTester.test ();
  CESKTester.test ();
