open! Core
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
      |}]

  let title = [%css {|
      color: #2e3a59;
      font-weight: 600;
      |}]
end

let run s =
  match Lib.SK.execute Lib.SK.init s with
  | _, value, _ -> Lib.SK.string_of_value value
  | exception e -> Exn.to_string e

let repl_component =
  let open Vdom.Node in
  let open Vdom.Attr in
  let%sub inp_state = Bonsai.state "" in
  let%sub history_state = Bonsai.state [] in

  let%arr inp, set_inp = inp_state and history, set_history = history_state in
  div ~attrs:[ Style.container ]
    [
      h3 ~attrs:[ Style.title ] [ text "REPL" ];
      form
        ~attrs:
          [
            on_submit (fun ev ->
                Js_of_ocaml.Dom.preventDefault ev;
                set_history ((">>> " ^ inp) :: run inp :: history));
          ]
        [
          input
            ~attrs:
              [
                type_ "text";
                value inp;
                on_input (Fn.const set_inp);
                placeholder "Type your code here...";
              ]
            ();
          button ~attrs:[ type_ "submit" ] [ text "Run" ];
        ];
      div
        [
          h4 [ text "Result:" ];
          div (List.map ~f:(fun hist -> pre [ text hist ]) history);
        ];
    ]

let () = Bonsai_web.Start.start repl_component
