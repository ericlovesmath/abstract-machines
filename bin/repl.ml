open Lib.Compiler

module type S = sig
  val start : unit -> 'a
  val print_execute : string -> unit
end

module Make(C : Compiler) : S = struct

  (** Enables multiline inputs by waiting for [\n\n] instead *)
  let multiline = ref false

  (** Enables (hopefully temporary) stdlib *)
  let base = ref false

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

  (** TODO: Don't put this here. Make an actual toplevel / #use, please *)
  let wrap_base program =
    if !base
      then "
        (let (max m n) (if (> m n) m n)
        (let (min m n) (if (< m n) m n)
        (let (mod m n) (- m (* n (/ m n)))
        (letrec (gcd m n) (if (= n 0) m (gcd n (mod m n)))
        (let (lcm m n) (* m (/ n (gcd m n)))

        (letrec (take n xs)
          (if (= n 0)
            nil
            (cons (car xs) (take (- n 1) (cdr xs))))

        (letrec (zipwith f xs ys)
          (cons (f (car xs) (car ys))
            (zipwith f (cdr xs) (cdr ys)))

        (letrec (filter f xs)
          (if (atom xs) nil
            (if (f (car xs))
              (cons (car xs) (filter f (cdr xs)))
              (filter f (cdr xs))))

        (letrec (map f xs)
          (if (atom xs) nil
            (cons (f (car xs)) (map f (cdr xs))))

        (letrec (fold f init xs)
          (if (atom xs) init
            (f (car xs) (fold f init (cdr xs))))

        (let (length xs) (fold (lambda (x acc) (+ acc 1)) 0 xs)
        " ^ program ^ ")))))))))))"
      else program

  (** Executes a [program] and prints out the result *)
  let print_execute program =
    program
    |> wrap_base
    |> C.execute !state
    |> fun (state', res) -> state := state'; res
    |> C.string_of_value
    |> print_endline

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
            ":b(ase) -> Toggle base mode, adds bindings for utility functions\n" ^
            ":r(un) <file> -> Executes contents of <file> as a string";
      | c when List.mem c [":m"; ":multi"] ->
          multiline := not !multiline;
          print_endline @@
            "Set multiline mode to " ^ string_of_bool !multiline;
      | c when List.mem c [":b"; ":base"] ->
          base := not !base;
          print_endline @@ if !base
            then "Including base (debug print will be polluted)"
            else "Not including base"
      | c when String.starts_with ~prefix:":r" c
            || String.starts_with ~prefix:":run" c ->
          ignore_err (fun () ->
            match String.split_on_char ' ' input with
            | [_; fname] ->
                print_execute (In_channel.with_open_text fname In_channel.input_all)
            | _ -> failwith "Unexpected :r syntax, See :h(elp)");
      | c when String.starts_with ~prefix:":" c ->
          print_endline "Error: Unexpected meta command, See :h(elp)";
      | _ ->
          ignore_err (fun () -> print_execute input);
    end;
    repl ()

  let start () =
    print_endline ("Welcome to the " ^ C.name ^ " REPL (type ':h(elp)' for meta commands)");
    repl ()
end
