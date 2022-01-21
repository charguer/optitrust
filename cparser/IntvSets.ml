open BinInt
open BinNums
open Coqlib

module ISet =
 struct
  module R =
   struct
    type t =
    | Nil
    | Cons of coq_Z * coq_Z * t

    (** val contains : coq_Z -> coq_Z -> t -> bool **)

    let rec contains l h = function
    | Nil -> false
    | Cons (l0, h0, s') ->
      (||) ((&&) ((fun x -> x) (zle h h0)) ((fun x -> x) (zle l0 l)))
        (contains l h s')

    (** val add : coq_Z -> coq_Z -> t -> t **)

    let rec add l h s = match s with
    | Nil -> Cons (l, h, Nil)
    | Cons (l0, h0, s') ->
      if zlt h0 l
      then Cons (l0, h0, (add l h s'))
      else if zlt h l0
           then Cons (l, h, s)
           else add (Z.min l0 l) (Z.max h0 h) s'

    (** val remove : coq_Z -> coq_Z -> t -> t **)

    let rec remove l h s = match s with
    | Nil -> Nil
    | Cons (l0, h0, s') ->
      if zlt h0 l
      then Cons (l0, h0, (remove l h s'))
      else if zlt h l0
           then s
           else if zlt l0 l
                then if zlt h h0
                     then Cons (l0, l, (Cons (h, h0, s')))
                     else Cons (l0, l, (remove l h s'))
                else if zlt h h0 then Cons (h, h0, s') else remove l h s'

    (** val inter : t -> t -> t **)

    let rec inter s1 s2 =
      let rec intr s3 =
        match s1 with
        | Nil -> Nil
        | Cons (l1, h1, s1') ->
          (match s3 with
           | Nil -> Nil
           | Cons (l2, h2, s2') ->
             if zle h1 l2
             then inter s1' s3
             else if zle h2 l1
                  then intr s2'
                  else if zle l1 l2
                       then if zle h2 h1
                            then Cons (l2, h2, (intr s2'))
                            else Cons (l2, h1, (inter s1' s3))
                       else if zle h1 h2
                            then Cons (l1, h1, (inter s1' s3))
                            else Cons (l1, h2, (intr s2')))
      in intr s2

    (** val beq : t -> t -> bool **)

    let rec beq s1 s2 =
      match s1 with
      | Nil -> (match s2 with
                | Nil -> true
                | Cons (_, _, _) -> false)
      | Cons (l1, h1, t1) ->
        (match s2 with
         | Nil -> false
         | Cons (l2, h2, t2) ->
           (&&) ((&&) ((fun x -> x) (zeq l1 l2)) ((fun x -> x) (zeq h1 h2)))
             (beq t1 t2))
   end

  type t = R.t

  (** val empty : t **)

  let empty =
    R.Nil

  (** val interval : coq_Z -> coq_Z -> t **)

  let interval l h =
    if zlt l h then R.Cons (l, h, R.Nil) else R.Nil

  (** val add : coq_Z -> coq_Z -> t -> t **)

  let add l h s =
    if zlt l h then R.add l h s else s

  (** val remove : coq_Z -> coq_Z -> t -> t **)

  let remove l h s =
    if zlt l h then R.remove l h s else s

  (** val inter : t -> t -> t **)

  let inter =
    R.inter

  (** val contains : coq_Z -> coq_Z -> t -> bool **)

  let contains l h s =
    if zlt l h then R.contains l h s else true

  (** val beq : t -> t -> bool **)

  let beq =
    R.beq
 end
