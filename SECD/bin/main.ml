open Sexp
open SECD

(* Simple Sanity Tests *)
let () =
  let test instrs = instrs |> SECD.init |> SECD.eval |> SECD.string_of_value |> print_endline in
  test @@ List [Atom MUL; List [Atom (Int 3); Atom LDC]; List [Atom ADD; List [Atom (Int 10); Atom LDC]; List [Atom (Int 2); Atom LDC]]];
