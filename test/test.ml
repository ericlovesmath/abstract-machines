open Lib

let base = [ "basics"; "higher-ordered"; "lambdas"; "letrec"; "fancy-examples" ]
let laziness = [ "lazy" ]
let imperative = [ "imperative" ]
let continuations = [ "call-cc" ]

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
  let files = base @ laziness
end)

module SKTester = Tester.Make (struct
  module C = SK
  let files = base
end)

module CESKTester = Tester.Make (struct
  module C = CESK
  let files = base @ imperative @ continuations
end)

let () =
  SECDTester.test ();
  CEKTester.test ();
  KrivineTester.test ();
  SKTester.test ();
  CESKTester.test ();
