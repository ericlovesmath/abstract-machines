(* ---------------------------------------------------------------------- 
 * S-expression pretty-printer
 * Stolen from Professor Michael Vanier (Caltech)
 * ---------------------------------------------------------------------- *)

(*

The overall approach is the following:

* Atoms are formatted as the corresponding string.
* For lists:
  - Try to format the list on a single line.
    If the line is less than some limit, accept it.
    Otherwise, reject it and format it on multiple lines.
  - If the list starts with another list,
    indent all list elements the same amount.
  - If the list starts with an atom,
    indent the rest of the list elements an extra `pp_indent` spaces.
*)

(* Seting [debug] to false disables all logs below *)
let debug = ref false

(* Beyond this length limit, lists have to be formatted on multiple lines. *)
let pp_line_limit = ref 60

(* Pretty-printer indent. *)
let pp_indent = ref 2

let rec flat_format (s : Sexplib.Sexp.t) =
  match s with
    | Atom s -> s
    | List l -> "(" ^ (String.concat " " (List.map flat_format l)) ^ ")"

(* Add indent to each item of a list. *)
let add_indent n lst =
  List.map (fun (i, s) -> (i + n, s)) lst

(* Add an open parenthesis to the front of the first line. 
   Add 1 to the indents of all other lines. *)
let add_open (lst : (int * string) list) : (int * string) list =
  match lst with
    | [] -> failwith "no items in list"
    | (n, s) :: t -> (n, "(" ^ s) :: add_indent 1 t

(* Add a close parenthesis to the end of the last line. *)
let rec add_close (lst : (int * string) list) : (int * string) list =
  match lst with
    | [] -> failwith "no items in list"
    | [(n, s)] -> [(n, s ^ ")")]
    | h :: t -> h :: add_close t

let rec sexp_format n s : (int * string) list =
  let flat = flat_format s in
    if String.length flat <= !pp_line_limit then
      [(n, flat)]
    else
      long_format n s

and long_format n s : (int * string) list =
  match s with
    | Atom s        -> [(n, s)]
    | List []       -> [(n, "()")]
    | List [Atom s] -> [(n, "(" ^ s ^ ")")]
    | List (Atom s :: rest) ->
      let first = (n, s) in
      let rest' = List.concat_map (sexp_format (n + !pp_indent - 1)) rest in
      let elts = first :: rest' in
        add_open (add_close elts)
    | List lst -> 
      let elts = List.concat_map (sexp_format n) lst in
        add_open (add_close elts)

let render_lines lines =
  let indent n = String.make n ' ' in
  let render_line (n, s) = indent n ^ s in
    String.concat "\n" (List.map render_line lines)

let pretty_print sexp =
  render_lines (sexp_format 0 sexp)

let print_pass label sexp =
  if !debug then begin
    Printf.eprintf "=== PASS: %s ===\n\n" label;
    Printf.eprintf "%s\n\n%!" (pretty_print sexp);
  end

let trace pass sexp_of v =
  print_pass pass (sexp_of v);
  v
