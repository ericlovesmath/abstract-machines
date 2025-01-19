open Lib

let _ =
  let ( == ) s expected =
    match Intro.parse s with
    | None -> failwith "Failed Parsing"
    | Some sexp -> assert (Intro.pp sexp = expected)
  in
  "(mul (add 5 2) 3)" == "mul(add(5, 2), 3)";
  "(mul (add +5 -2) 003)" == "mul(add(5, -2), 3)";
  "(mul (add ((one)) -2) 003)" == "mul(add(one, -2), 3)"
