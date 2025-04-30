open Bonsai_web
open Bonsai.Let_syntax

module Style = struct
  let container =
    [%css
      {|
      max-width: 600px;
      margin: 2rem;
      padding: 2rem;
      background: #eee;
      border-radius: 1rem;
      font-family: "Fira Mono", "Consolas", monospace;
      |}
    ]

  let title =
    [%css
      {|
      color: #2e3a59;
      font-weight: 600;
      |}
    ]
end

let run s = 
  try
    Lib.Krivine.string_of_value (Lib.Krivine.execute s)
  with e ->
    Printf.sprintf "Error: %s\n" (Printexc.to_string e)

let repl_component =
  let%sub inp, set_inp = Bonsai.state "" in
  let%sub history, set_history = Bonsai.state [] in

  return
    (let%map inp = inp
     and set_inp = set_inp
     and history = history
     and set_history = set_history in
     let open Vdom.Node in
     let open Vdom.Attr in
     div ~attrs:[ Style.container ]
       [ h3 ~attrs:[ Style.title ] [ text "REPL" ]
       ; form
           ~attrs:[ on_submit (fun ev ->
             Js_of_ocaml.Dom.preventDefault ev;
             set_history ((">>> " ^ inp) :: run inp :: history)
           ) ]
           [ input
               ~attrs:
                 [ type_ "text"
                 ; value inp
                 ; on_input (Fun.const set_inp)
                 ; placeholder "Type your code here..."
                 ]
               ()
           ; button
               ~attrs:[ type_ "submit" ]
               [ text "Run" ]
           ]
       ; div
           [ h4 [ text "Result:" ]
           ; div (List.map (fun hist -> pre [ text hist ]) history)
           ]
       ]
    )

let () = Bonsai_web.Start.start repl_component
