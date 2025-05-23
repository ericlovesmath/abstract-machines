open Lib

let help = "Usage: dune exec abstract_machines -- [-machine MACHINE] [-file FILENAME|-code CODE] [-debug]"

module SECDRepl = Repl.Make(SECD)
module CEKRepl = Repl.Make(CEK)
module KrivineRepl = Repl.Make(Krivine)
module SKRepl = Repl.Make(SK)
module CESKRepl = Repl.Make(CESK)

let () =
  let machine = ref "" in
  let filename = ref "" in
  let code = ref "" in
  let speclist =
    [
      ("-machine", Arg.Set_string machine, "Target Backend");
      ("-file", Arg.Set_string filename, "File to Execute");
      ("-code", Arg.Set_string code, "String to Execute");
      ("-debug", Arg.Set Debug.debug, "Print Compiler Passes");
    ]
  in

  Arg.parse speclist (Fun.const ()) help;

  let backend =
    match !machine with
    | "secd" | "SECD" -> (module SECDRepl : Repl.S)
    | "cek" | "CEK" -> (module CEKRepl : Repl.S)
    | "krivine" | "Krivine" -> (module KrivineRepl : Repl.S)
    | "sk" | "SK" -> (module SKRepl : Repl.S)
    | "cesk" | "CESK" -> (module CESKRepl : Repl.S)
    | _ -> failwith "Invalid compiler specified. Use 'secd', 'cek', 'krivine', 'sk', or 'cesk'."
  in

  let module REPL = (val backend : Repl.S) in

  match (!filename, !code) with
  | "", "" -> REPL.start ()
  | "", code -> REPL.print_execute code
  | path, _ ->
      let open In_channel in
      REPL.print_execute (input_all (open_text path))
