let unwrap (err : string) (opt : 'a option)  : 'a =
  match opt with
  | None -> failwith err
  | Some x -> x

let parse program = unwrap "Error: Parser fail" (Intro.parse program)

let execute (program : string) : Krivine.value =
  program
  |> parse
  |> Parse.parse
  |> Krivine.eval
  |> Krivine.force
