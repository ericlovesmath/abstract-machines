open Lib

let run_test path =
  try
    let file = In_channel.open_text path in
    let expected = Option.get (In_channel.input_line file) in
    let source = In_channel.input_all file in
    assert (CEK.string_of_value (Compile.execute source) = expected)
  with e ->
    print_endline ("Error in " ^ path);
    raise e

let () =
  Array.iter
    (fun f -> run_test ("tests/" ^ f))
    (Sys.readdir "tests")
