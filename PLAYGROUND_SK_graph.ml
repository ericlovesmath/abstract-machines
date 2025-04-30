type vertex = int

type label =
  | S
  | K
  | I
  | Int of int
  | Add
  | Bool of bool
  | If
  | Lt
  | App of vertex * vertex

let label_tbl : (vertex, label) Hashtbl.t = Hashtbl.create 32

let counter = ref 0
let fresh_vertex label =
  let v = !counter in
  incr counter;
  Hashtbl.add label_tbl v label;
  v

let mk_s () = fresh_vertex S
let mk_k () = fresh_vertex K
let mk_i () = fresh_vertex I
let mk_int n = fresh_vertex (Int n)
let mk_add () = fresh_vertex Add
let mk_bool b = fresh_vertex (Bool b)
let mk_if () = fresh_vertex If
let mk_lt () = fresh_vertex Lt
let mk_app l r = fresh_vertex (App (l, r))

let rec to_string v =
  match Hashtbl.find label_tbl v with
  | S -> "S"
  | K -> "K"
  | I -> "I"
  | Int n -> Int.to_string n
  | Add -> "+"
  | Bool true -> "#t"
  | Bool false -> "#f"
  | If -> "if"
  | Lt -> "<"
  | App (l, r) -> "(" ^ to_string l ^ " " ^ to_string r ^ ")"

let rec reduce v =
  match Hashtbl.find label_tbl v with
  | App (f, z) -> begin
      match Hashtbl.find label_tbl f with
      | App (g, y) -> begin
          match Hashtbl.find label_tbl g with
          | App (h, x) -> begin
              match Hashtbl.find label_tbl h with
              | S ->
                  let xz = mk_app x z in
                  let yz = mk_app y z in
                  let new_app = mk_app xz yz in
                  Hashtbl.replace label_tbl v (Hashtbl.find label_tbl new_app);
                  reduce v
              | If ->
                  begin
                    match Hashtbl.find label_tbl x with
                    | Bool b ->
                        Hashtbl.replace label_tbl v (Hashtbl.find label_tbl (if b then y else z));
                        reduce v
                    | _ -> reduce f; reduce z
                  end
              | _ -> reduce f; reduce z
          end
          | K ->
              Hashtbl.replace label_tbl v (Hashtbl.find label_tbl y);
              reduce v
          | Add ->
              begin
                match (Hashtbl.find label_tbl y, Hashtbl.find label_tbl z) with
                | Int n, Int m ->
                    Hashtbl.replace label_tbl v (Int (n + m));
                    reduce v
                | _ -> reduce f; reduce z
              end
          | Lt ->
              begin
                match (Hashtbl.find label_tbl y, Hashtbl.find label_tbl z) with
                | Int n, Int m ->
                    Hashtbl.replace label_tbl v (Bool (n < m));
                    reduce v
                | _ -> reduce f; reduce z
              end
          | _ -> reduce f; reduce z
      end
      | I ->
          Hashtbl.replace label_tbl v (Hashtbl.find label_tbl z);
          reduce v
      | _ -> reduce f; reduce z
  end
  | _ -> ()

let rec normalize v =
  let before = to_string v in
  reduce v;
  let after = to_string v in
  if before = after then v
  else normalize v

let truncate s max_len =
  if String.length s <= max_len then s
  else String.sub s 0 (max 0 (max_len - 3)) ^ "..."

let test expr expected = 
  Printf.printf "%s\n" (truncate (to_string expr) 55);
  let _ = normalize expr in
  if (to_string expr <> to_string expected)
    then (
      Printf.printf "> Recieved:  %s\n" (to_string expr);
      Printf.printf "> Expected:  %s\n" (to_string expected))

let () =
  let s = mk_s () in
  let k = mk_k () in
  let i = mk_i () in
  let ( $ ) = mk_app in

  (* Basic tests *)
  test (i $ i) i;
  test (k $ k $ i) k;
  test (s $ k $ s $ i) i;
  test (s $ k $ k $ i) i;
  test (s $ k $ i $ k) k;
  test (k $ s $ i) s;
  test (s $ k $ i $ (k $ i $ s)) i;
  test (k $ s $ (i $ (s $ k $ s $ i))) s;

  let f = s $ k in
  let t = k in
  let notb b = b $ f $ t in
  let orb b b' = b $ t $ b' in
  let andb b b' = b $ b' $ f in

  (* Boolean tests *)
  test (notb f) t;
  test (notb t) f;

  test (orb t t) t;
  test (orb t f) t;
  test (orb f t) t;
  test (orb f f) f;

  test (andb t t) t;
  test (andb t f) f;
  test (andb f t) f;
  test (andb f f) f;

  test (orb (andb t f) (andb t t)) t;

  let zero = s $ k in
  let succ = s $ (s $ (k $ s) $ k) in
  let to_unary church = church $ s $ s in
  let rec of_int n = if n = 0 then s else (s $ of_int (n - 1)) in

  (* Church Numerals, applying [n S S] to get value *)
  test (to_unary zero) (of_int 0);
  test (to_unary (succ $ zero)) (of_int 1);
  test (to_unary (succ $ (succ $ zero))) (of_int 2);
  test (to_unary (succ $ (succ $ (succ $ zero)))) (of_int 3);

  (* Adding Church Numerals *)
  let three = succ $ (succ $ (succ $ zero)) in
  let add n n' = n $ succ $ n' in
  test (to_unary (add three three)) (of_int 6);

  (* Multiplying Church Numerals *)
  let four = succ $ (succ $ (succ $ (succ $ zero))) in
  let mul n n' = n $ (n' $ succ) $ zero in
  test (to_unary (mul three four)) (of_int 12);

  (* exp x y = y x, so exp = swap *)
  let swap = s $ (k $ (s $ i)) $ (s $ (k $ k) $ i) in
  test (to_unary (swap $ three $ zero)) (of_int 1);
  test (to_unary (swap $ three $ four)) (of_int 81);
  test (to_unary (swap $ four $ three)) (of_int 64);

  (* Laziness test, since [huge] takes forever to evaluate *)
  let huge = swap $ (swap $ four $ three) $ four in
  test (to_unary (swap $ huge $ zero)) (of_int 1);

  (* Arithmetic, (+ (+ 1 2) (+ 3 4)) *)
  let n = mk_int in
  let add = mk_add () in
  test (add $ (add $ n 1 $ n 2) $ (add $ n 3 $ n 4)) (n 10);

  (* If, Bools, < *)
  let b = mk_bool in
  let lt = mk_lt () in
  let cond = mk_if () in
  test (cond $ b true $ n 1 $ n 2) (n 1);
  test (cond $ b false $ n 1 $ n 2) (n 2);
  test (cond $ b true $ (add $ n 1 $ n 3) $ n 2) (n 4);
  test (cond $ (lt $ (add $ n 1 $ n 3) $ (n 10)) $ n 1 $ n 2) (n 1);
