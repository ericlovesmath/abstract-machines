open Lib

let () =
  let run instrs = instrs |> SECD.init |> SECD.eval |> SECD.string_of_value in
  assert (
    "36"
    = run
      @@ List
           [
             Atom MUL;
             List [ Atom (Int 3); Atom LDC ];
             List
               [
                 Atom ADD;
                 List [ Atom (Int 10); Atom LDC ];
                 List [ Atom (Int 2); Atom LDC ];
               ];
           ])
