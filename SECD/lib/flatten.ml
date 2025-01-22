let flatten (ast : Intro.t) : SECD.instr list =
  let rec aux (acc : SECD.instr list) (ast : Intro.t) : SECD.instr list=
    match ast with
    | Nil -> NIL :: acc
    | Int i -> LDC :: Int i :: acc
    | Var _ -> failwith "flatten: TODO"
    | If (c, t, f) -> aux (SEL :: List (aux [JOIN] t) :: List (aux [JOIN] f) :: acc) c
    | Call (Prim _ as prim, xs) -> List.fold_left aux (aux acc prim) xs
    | Call _ -> failwith "flatten: TODO"
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
