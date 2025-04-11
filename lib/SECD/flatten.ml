let flatten (ast : Assign.t) : Machine.instr list =
  let rec aux (acc : Machine.instr list) (ast : Assign.t) : Machine.instr list=
    match ast with
    | Nil -> NIL :: acc
    | Int i -> LDC :: Int i :: acc
    | Bool b -> LDC :: Bool b :: acc
    | If (c, t, f) -> aux (SEL :: List (aux [JOIN] t) :: List (aux [JOIN] f) :: acc) c
    | Call (Prim _ as prim, xs) -> List.fold_left aux (aux acc prim) xs

    | Call (f, xs) ->
        let acc = aux (AP :: acc) f in
        NIL :: List.fold_left (fun acc x -> aux (CONS :: acc) x) acc xs
    | CallRec (f, xs) ->
        let acc = aux (RAP :: acc) f in
        NIL :: List.fold_left (fun acc x -> aux (CONS :: acc) x) acc xs
    | Lambda b -> LDF :: List (aux [RTN] b) :: acc
    | LambdaRec b -> DUM :: LDF :: List (aux [RTN] b) :: acc
    | Loc (x, y) -> LD :: Int x :: Int y :: acc

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
