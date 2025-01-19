open Lib

let _ =
  let ( == ) s expected =
    match Sexp.parse s with
    | None -> failwith "Failed Parsing"
    | Some sexp -> assert (Sexp.pp Fun.id sexp = expected)
  in
  "(   - ( +  1  2)    3)" == "(- (+ 1 2) 3)";
  "(+ - / * ab cde 123)" == "(+ - / * ab cde 123)";
  "(+ (- x y) (- +1 -3) ((a b) c))" == "(+ (- x y) (- +1 -3) ((a b) c))"

let _ =
  let fail s = assert (Sexp.parse s = None) in
  fail "";
  fail "(";
  fail ")";
  fail "()";
  fail "(12ab)";
  fail "(12)(12)";
