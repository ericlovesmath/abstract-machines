let flatten (ast : Assign.t) : SECD.instr list =
  let rec aux (acc : SECD.instr list) (ast : Assign.t) : SECD.instr list=
    match ast with
    | Nil -> NIL :: acc
    | Int i -> LDC :: Int i :: acc
    | If (c, t, f) -> aux (SEL :: List (aux [JOIN] t) :: List (aux [JOIN] f) :: acc) c
    | Call (Prim _ as prim, xs) -> List.fold_left aux (aux acc prim) xs

    (* Assume Rec is directly above idk *)
    | Rec -> LD :: Int 1 :: Int 0 :: acc

    | Call (f, xs) ->
        let acc = aux (AP :: acc) f in
        NIL :: List.fold_left (fun acc x -> aux (CONS :: acc) x) acc xs
    | Lambda b -> LDF :: List (aux [RTN] b) :: acc
    | Loc (x, y) -> LD :: Int x :: Int y :: acc

    | CallRec (_fname, f, xs) ->
        let acc = aux (RAP :: acc) f in
        DUM :: NIL :: List.fold_left (fun acc x -> aux (CONS :: acc) x) acc xs

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
