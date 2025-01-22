let flatten (ast : Intro.t) : SECD.instr list =
  let rec aux (acc : SECD.instr list) (ast : Intro.t) : SECD.instr list=
    match ast with
    | Nil -> NIL :: acc
    | Int i -> LDC :: Int i :: acc
    | If (c, t, f) -> aux (SEL :: List (aux [JOIN] t) :: List (aux [JOIN] f) :: acc) c
    | Call (Prim _ as prim, xs) -> List.fold_left aux (aux acc prim) xs

    (* TODO: For now, assume its all lambda functions applied
       and that variables aren't ever enclosed. And they have one arg. *)
    | Call (f, xs) ->
        let acc = aux (AP :: acc) f in
        NIL :: List.fold_left (fun acc x -> aux (CONS :: acc) x) acc xs
    | Lambda (_, b) -> LDF :: List (aux [RTN] b) :: acc
    | Var _ -> LD :: Int 0 :: Int 0 :: acc

    | Prim Add -> ADD :: acc
    | Prim Sub -> SUB :: acc
    | Prim Mul -> MUL :: acc
    | Prim Div -> DIV :: acc
    | Prim Atom -> ATOM :: acc
    | Prim Cons -> CONS :: acc
    | Prim Car -> CAR :: acc
    | Prim Cdr -> CDR :: acc
    | Prim Eq -> EQ :: acc
    | Prim Lt -> LT :: acc
    | Prim Gt -> GT :: acc
    | Prim Le -> LE :: acc
    | Prim Ge -> GE :: acc
  in
  aux [] ast
