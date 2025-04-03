open Lib
open Compiler

module type Tester = sig
  val test : unit -> unit
end

module type Testable = sig
  module C : Compiler
  val files : string list
end

module Make (T : Testable) : Tester = struct
  let run_test source =
    let lines = String.split_on_char '\n' source in
    (match lines with
    | [] -> failwith "Error: Expected test to be nonempty"
    | output :: expr ->
        let expr = String.concat "\n" expr in
        if String.starts_with ~prefix:"OUTPUT: " output
        then
          let output = String.sub output 8 (String.length output - 8) in
          let res = T.C.string_of_value (T.C.execute expr) in
          (if res <> output
            then failwith (Printf.sprintf "Error: Expected %s, got %s" output res))
        else if output = "FAIL"
        then
          let failed =
            try (ignore (T.C.execute expr); false)
            with _ -> true
          in
          (if not failed
            then failwith "Expected program to fail")
        else
          failwith "Error: Test in unexpected format");
    print_char '#'

  let run_file path =
    Printf.printf "> Testing %s: " path;
    try
      let file = In_channel.open_text path in
      let source = In_channel.input_all file in
      let tests = Str.split (Str.regexp "// ") source in
      List.iter run_test tests;
      print_char '\n'
    with e ->
      print_endline ("Error in " ^ path);
      raise e

  let test () =
    print_endline ("Running " ^ T.C.name ^ " tests...\n");
    List.iter (fun f -> run_file ("tests/" ^ f ^ ".src")) T.files;
    print_endline "\nAll tests pass!\n"
end
