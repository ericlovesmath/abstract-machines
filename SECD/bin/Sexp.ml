type 'a t = List of 'a t list | Atom of 'a

let flatten (sexp : 'a t) : 'a list =
  let rec aux (acc : 'a list) (sexp : 'a t) : 'a list =
    match sexp with
      | List [] -> acc
      | List (e :: es) -> aux (aux acc e) (List es)
      | Atom a -> a :: acc
  in
  aux [] sexp
