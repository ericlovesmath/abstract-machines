let unwrap (err : string) (opt : 'a option)  : 'a =
  match opt with
  | None -> failwith err
  | Some x -> x

let parse program = unwrap "Error: Parser fail" (Intro.parse program)
let assign_vars = Assign.assign_vars
let flatten = Flatten.flatten
let secd instrs = SECD.eval (SECD.init instrs)

let execute (program : string) : SECD.value =
  program
  |> parse
  |> assign_vars
  |> flatten
  |> secd
