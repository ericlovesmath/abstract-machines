open Machine

let counter = ref 0

let genvar () =
  let v = "$anf.tmp" ^ string_of_int !counter in
  counter := !counter + 1;
  v

let is_atomic (e : t) : bool =
  match e with
  | Nil | Int _ | Bool _ | Var _ | Fn _ -> true
  | _ -> false

let anf (e : t) : t =

  (* CPS Style convert [e] to atomic value, then continue [k] *)
  let rec atomize (e : t) (k : t -> t) : t =
    anf' e (fun e ->
      if is_atomic e
        then k e
        else
          let var = genvar () in
          Let (var, e, k (Var var)))

  (* Monadic Binding for Continuation Passing Style *)
  and ( let* ) e k = atomize e (fun e' -> k e')

  and anf' (e : t) (k : t -> t) : t =
    let make_binop cons e e' =
      let* e = e in
      let* e' = e' in
      k (cons e e')
    in
    match e with
    | Nil
    | Int _
    | Bool _
    | Var _ -> k e

    | Let (v, bind, e) ->
        let* bind = bind in
        k (Let (v, bind, anf' e Fun.id))

    | If (c, t, f) ->
        let* c = c in
        If (c, anf' t k, anf' f k)

    | Fn (params, b) ->
        k (Fn (params, anf' b Fun.id))
    | Rec (f, params, b) ->
        k (Rec (f, params, anf' b Fun.id))

    | Call (f, args) ->
        (* CPS Binding for [atomize] on list of elements *)
        let rec ( let+ ) args k =
          match args with
          | [] -> k []
          | h :: t ->
              let* h = h in
              let+ t = t in
              k (h :: t)
        in
        let* f = f in
        let+ args = args in
        k (Call (f, args))

    | Set (x, e) ->
        let* e = e in
        k (Set (x, e))

    | Begin es ->
        let rec ( let+ ) args k =
          match args with
          | [] -> k []
          | h :: t ->
              let* h = h in
              let+ t = t in
              k (h :: t)
        in
        let+ es = es in
        k (Begin es)

    | Add (e, e') -> make_binop (fun x y -> Add (x, y)) e e'
    | Sub (e, e') -> make_binop (fun x y -> Sub (x, y)) e e'
    | Mul (e, e') -> make_binop (fun x y -> Mul (x, y)) e e'
    | Div (e, e') -> make_binop (fun x y -> Div (x, y)) e e'

    | Lt (e, e') -> make_binop (fun x y -> Lt (x, y)) e e'
    | Gt (e, e') -> make_binop (fun x y -> Gt (x, y)) e e'
    | Le (e, e') -> make_binop (fun x y -> Le (x, y)) e e'
    | Ge (e, e') -> make_binop (fun x y -> Ge (x, y)) e e'
    | Eq (e, e') -> make_binop (fun x y -> Eq (x, y)) e e'

    | Atom e -> let* e = e in k (Atom e)
    | Cons (e, e') -> make_binop (fun x y -> Cons (x, y)) e e'
    | Car e -> let* e = e in k (Car e)
    | Cdr e -> let* e = e in k (Cdr e)
  in
  anf' e Fun.id
