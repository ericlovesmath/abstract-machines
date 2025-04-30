open Lib

let base =
  [ "basics"; "fails"; "higher-ordered"; "lambdas"; "y-combinator"; "letrec" ]

module SECDTester = Tester.Make (struct
  module C = SECD
  let files = base
end)

module CEKTester = Tester.Make (struct
  module C = CEK
  let files = base @ ["fancy"]
end)

module KrivineTester = Tester.Make (struct
  module C = Krivine
  let files = base @ ["fancy"; "lazy"]
end)

module SKTester = Tester.Make (struct
  module C = SK
  let files = [ "basics"; "lambdas"; "higher-ordered" ]
end)

let () =
  SECDTester.test ();
  CEKTester.test ();
  KrivineTester.test ();
  SKTester.test ();
  print_endline "Note: SK Tests are non-comprehensive"
