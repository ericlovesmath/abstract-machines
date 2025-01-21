open Lib

let rec repl () =
  print_string ">>> ";
  let input = read_line () in
  if input = ":q" then
    print_endline "Goodbye!"
  else
    try
      print_endline (SECD.string_of_value (Compile.execute input));
      repl ()
    with e ->
      Printf.printf "Error: %s\n" (Printexc.to_string e);
      repl ()

let () =
  print_endline "Welcome to the SECD REPL (type ':q' to quit)";
  repl ()
