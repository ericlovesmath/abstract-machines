open Lib

let unwrap (err : string) (opt : 'a option)  : 'a =
  match opt with
  | None -> failwith err
  | Some x -> x

let execute (p : string) : string =
  p
  |> Intro.parse
  |> unwrap "failed to parse"
  |> Flatten.flatten
  |> SECD.init
  |> SECD.eval
  |> SECD.string_of_value

let rec repl () =
  print_string ">>> ";
  let input = read_line () in
  if input = ":q" then
    print_endline "Goodbye!"
  else
    try
      print_endline (execute input);
      repl ()
    with e ->
      Printf.printf "Error: %s\n" (Printexc.to_string e);
      repl ()

let () =
  print_endline "Welcome to the SECD REPL (type ':q' to quit)";
  repl ()
