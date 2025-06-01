open Lib.Compiler

module type S = sig
  val start : unit -> 'a
  val print_execute : string -> unit
end

module Make(C : Compiler) : S = struct

  (** Enables multiline inputs by waiting for [\n\n] instead *)
  let multiline = ref false

  (** Stores state to have true toplevel REPL *)
  let state = ref C.init

  (** Executes [f] but prints [err] on error, for REPL purposes *)
  let ignore_err f =
      try
        f ()
      with e ->
        Printf.printf "Error: %s\n" (Printexc.to_string e)

  (** Gets line for stdin, waits for double newlines if [multiline] is on *)
  let rec get_input () =
    let line = read_line () in
    if !multiline && line <> "" && String.get line 0 <> ':' then
      line ^ "\n" ^ get_input ()
    else
      line

  (** Executes a [program] and prints out the result *)
  let rec print_execute program =
    let (state', res, rem) = C.execute !state program in
    state := state';
    print_endline (C.string_of_value res);
    if rem <> "" then print_execute rem

  let rec repl () =
    print_string ">>> ";
    let input = get_input () in
    begin
      match input with
      | c when List.mem c [":q"; ":quit"] ->
          print_endline "Goodbye!";
          exit 0
      | c when List.mem c [":h"; ":help"] ->
          print_endline @@
            ":h(elp) -> View this message\n" ^
            ":q(uit) -> Exit REPL\n" ^
            ":m(ulti) -> Toggle multiline mode, uses double newlines to run\n" ^
            ":l(oad) <file> -> Executes contents of <file> as a string"
      | c when List.mem c [":m"; ":multi"] ->
          multiline := not !multiline;
          print_endline @@
            "Set multiline mode to " ^ string_of_bool !multiline;
      | c when String.starts_with ~prefix:":l" c
            || String.starts_with ~prefix:":load" c ->
          ignore_err (fun () ->
            match String.split_on_char ' ' input with
            | [_; fname] ->
                print_execute In_channel.(with_open_text fname input_all)
            | _ -> failwith "Unexpected :l syntax, See :h(elp)");
      | c when String.starts_with ~prefix:":" c ->
          print_endline "Error: Unexpected meta command, See :h(elp)";
      | "" -> ()
      | _ ->
          ignore_err (fun () -> print_execute input);
    end;
    repl ()

  let start () =
    print_endline ("Welcome to the " ^ C.name ^ " REPL (type ':h(elp)' for meta commands)");
    repl ()
end
