open Lib
open Compiler

let test (module C : Compiler) (files : string list) =

  let run_test file =
    let state = ref C.init in

    let rec test program =
      let (state', _, rem) = C.execute !state program in
      state := state';
      if rem <> "" then test rem
    in

    let read (file : string) =
      In_channel.(with_open_text ("tests/" ^ file ^ ".scm") input_all)
    in

    print_endline ("- " ^ file);
    test (read file)
  in

  print_endline ("Running " ^ C.name ^ " tests...");
  List.iter run_test files;
  print_endline ""

let base = [ "basics"; "lambdas"; "higher-ordered"; "toplevel"; "letrec"; "fancy-examples" ]

let _ =
  test (module SECD : Compiler)    base;
  test (module CEK : Compiler)     base;
  test (module CESK : Compiler)    (base @ [ "imperative"; "call-cc" ]);
  test (module Krivine : Compiler) (base @ [ "lazy" ]);
  test (module SK : Compiler)      base;
  print_endline "All tests pass!"
