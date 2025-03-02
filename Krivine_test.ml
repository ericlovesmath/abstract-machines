let y_combinator =
  Lambda
    ( "f",
      App
        ( Lambda ("x", App (Var "f", App (Var "x", Var "x"))),
          Lambda ("x", App (Var "f", App (Var "x", Var "x"))) ) )

let ones =
  App (y_combinator, Lambda ("ones", App (App (Cons, Int 1), Var "ones")))

let nats =
  let ints_from_n =
    Lambda
      ( "ints_from_n",
        Lambda
          ( "n",
            App
              ( App (Cons, Var "n"),
                App (Var "ints_from_n", App (App (Add, Var "n"), Int 1)) ) ) )
  in
  App (App (y_combinator, ints_from_n), Int 1)

let ints =
  let ints_from =
    Lambda
      ( "self",
        Lambda
          ( "n",
            App
              ( App (Cons, Var "n"),
                App
                  (App (Var "self", Var "self"), App (App (Add, Var "n"), Int 1))
              ) ) )
  in
  App (App (ints_from, ints_from), Int 1)

let f =
  App
    ( Lambda ("inc", (* let inc = ... in ... *)
                     App (Var "inc", Int 10)),
      (* inc 10 *)
      Lambda ("x", (* λx. x + 1 *)
                   App (Var "inc", Var "x")) )


let _ = force @@ result @@ App (Head, App (Tail, ones))
let _ = force @@ result @@ App (Head, App (Tail, ints))
let _ = force @@ result @@ App (Head, App (Tail, App (Tail, nats)))
