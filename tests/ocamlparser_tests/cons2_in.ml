

type li =
        | Nil
        | Cons of int * li
        | Cons2 of int * int * li


(*
let init len f =
 let rec init_aux i f =
    if i > (len - 1) then Nil
    else if i = (len - 1) then Cons (f i, Nil)
    else
      Cons2 (f i, f (i+1), init_aux (i+2) f)
in
init_aux 0 f

let rec map f l =
    match l with
    | Nil -> Nil
    | Cons (x, l') -> Cons (f x, map f l')
    | Cons2 (x, y, l'') -> Cons2 (f x, f y, map f l'')

let rec iter f l =
  match l with
  | Nil -> ()
  | Cons (x, t) -> f x; iter f t
  | Cons2 (x, y, u) -> f x; f y; iter f u
*)
