
module BE =
 struct
  (** val dec_eq_from_bool_eq :
      ('a1 -> 'a1 -> bool) -> 'a1 -> 'a1 -> bool **)

  let dec_eq_from_bool_eq f x y =
    let b = f x y in if b then true else false
 end
