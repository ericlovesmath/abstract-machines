open Lib

let help = "Usage: dune exec CEK -- [-file FILENAME] [-repl]"

module CEKRepl = Repl.Make(Compiler.CEK)
module SECDRepl = Repl.Make(Compiler.CEK)

let () =
  let machine = ref "" in
  let filename = ref "" in
  let code = ref "" in
  let speclist =
    [
      ("-machine", Arg.Set_string machine, "Target Machine");
      ("-file", Arg.Set_string filename, "File to Execute");
      ("-code", Arg.Set_string code, "String to Execute");
    ]
  in

  Arg.parse speclist (Fun.const ()) help;

  let backend =
    match !machine with
    | "cek" | "CEK" -> (module CEKRepl : Repl.S)
    | "secd" | "SECD" -> (module SECDRepl : Repl.S)
    | _ -> failwith "Invalid compiler specified. Use 'cek' or 'secd'."
  in

  let module REPL = (val backend : Repl.S) in

  match (!filename, !code) with
  | "", "" -> REPL.start ()
  | "", code -> REPL.print_execute code
  | path, _ ->
      let open In_channel in
      REPL.print_execute (input_all (open_text path))
