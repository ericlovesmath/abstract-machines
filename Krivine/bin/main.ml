let help = "Usage: dune exec Krivine -- [-file FILENAME] [-repl]"

let () =
  let repl = ref false in
  let filename = ref "" in
  let code = ref "" in
  let speclist =
    [
      ("-repl", Arg.Set repl, "REPL Mode");
      ("-file", Arg.Set_string filename, "File to Execute");
      ("-code", Arg.Set_string code, "String to Execute");
    ]
  in

  Arg.parse speclist (Fun.const ()) help;

  match (!repl, !filename, !code) with
  | true, _, _ -> Repl.start_repl ()
  | _, "", "" -> Arg.usage speclist help
  | _, "", code -> Repl.print_execute code
  | _, path, _ ->
      let open In_channel in
      Repl.print_execute (input_all (open_text path))
