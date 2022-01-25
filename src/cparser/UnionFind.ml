open Datatypes

type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

module type MAP =
 sig
  type elt

  val elt_eq : elt -> elt -> bool

  type 'x t

  val empty : 'a1 t

  val get : elt -> 'a1 t -> 'a1 option

  val set : elt -> 'a1 -> 'a1 t -> 'a1 t
 end

module type UNIONFIND =
 sig
  type elt

  val elt_eq : elt -> elt -> bool

  type t

  val repr : t -> elt -> elt

  val empty : t

  val find : t -> elt -> elt * t

  val union : t -> elt -> elt -> t

  val merge : t -> elt -> elt -> t

  val pathlen : t -> elt -> nat
 end

module UF =
 functor (M:MAP) ->
 struct
  type elt = M.elt

  (** val elt_eq : M.elt -> M.elt -> bool **)

  let elt_eq =
    M.elt_eq

  type unionfind = elt M.t
    (* singleton inductive, whose constructor was mk *)

  (** val m : unionfind -> elt M.t **)

  let m u =
    u

  type t = unionfind

  (** val getlink : elt M.t -> elt -> elt option **)

  let getlink m0 a =
    let o = M.get a m0 in (match o with
                           | Some e -> Some e
                           | None -> None)

  (** val coq_F_repr : t -> elt -> (elt -> __ -> elt) -> elt **)

  let coq_F_repr uf a rec0 =
    match getlink (m uf) a with
    | Some s -> rec0 s __
    | None -> a

  (** val repr : t -> elt -> elt **)

  let rec repr uf a =
    coq_F_repr uf a (fun y _ -> repr uf y)

  (** val repr_canonical : __ **)

  let repr_canonical =
    __

  type sameclass = __

  (** val sameclass_refl : __ **)

  let sameclass_refl =
    __

  (** val sameclass_sym : __ **)

  let sameclass_sym =
    __

  (** val sameclass_trans : __ **)

  let sameclass_trans =
    __

  (** val sameclass_repr : __ **)

  let sameclass_repr =
    __

  (** val empty : t **)

  let empty =
    M.empty

  (** val repr_empty : __ **)

  let repr_empty =
    __

  (** val sameclass_empty : __ **)

  let sameclass_empty =
    __

  (** val identify : t -> elt -> elt -> unionfind **)

  let identify uf a b =
    M.set a b (m uf)

  (** val union : t -> elt -> elt -> t **)

  let union uf a b =
    let a' = repr uf a in
    let b' = repr uf b in if M.elt_eq a' b' then uf else identify uf a' b'

  (** val repr_union_1 : __ **)

  let repr_union_1 =
    __

  (** val repr_union_2 : __ **)

  let repr_union_2 =
    __

  (** val repr_union_3 : __ **)

  let repr_union_3 =
    __

  (** val sameclass_union_1 : __ **)

  let sameclass_union_1 =
    __

  (** val sameclass_union_2 : __ **)

  let sameclass_union_2 =
    __

  (** val sameclass_union_3 : __ **)

  let sameclass_union_3 =
    __

  (** val merge : t -> elt -> elt -> t **)

  let merge uf a b =
    let a' = repr uf a in
    let b' = repr uf b in if M.elt_eq a' b' then uf else identify uf a' b

  (** val repr_merge : __ **)

  let repr_merge =
    __

  (** val sameclass_merge : __ **)

  let sameclass_merge =
    __

  type path_ord = __

  (** val path_ord_wellfounded : __ **)

  let path_ord_wellfounded =
    __

  (** val path_ord_canonical : __ **)

  let path_ord_canonical =
    __

  (** val path_ord_merge_1 : __ **)

  let path_ord_merge_1 =
    __

  (** val path_ord_merge_2 : __ **)

  let path_ord_merge_2 =
    __

  (** val coq_F_pathlen : t -> elt -> (elt -> __ -> nat) -> nat **)

  let coq_F_pathlen uf a rec0 =
    match getlink (m uf) a with
    | Some s -> S (rec0 s __)
    | None -> O

  (** val pathlen : t -> elt -> nat **)

  let rec pathlen uf a =
    coq_F_pathlen uf a (fun y _ -> pathlen uf y)

  (** val pathlen_zero : __ **)

  let pathlen_zero =
    __

  (** val pathlen_merge : __ **)

  let pathlen_merge =
    __

  (** val pathlen_gt_merge : __ **)

  let pathlen_gt_merge =
    __

  (** val compress : t -> elt -> elt -> unionfind **)

  let compress uf a b =
    M.set a b (m uf)

  (** val find_x : t -> elt -> (elt * t) **)

  let rec find_x uf x =
    let filtered_var = M.get x (m uf) in
    (match filtered_var with
     | Some a' ->
       let filtered_var0 = find_x uf a' in
       let (b, uf') = filtered_var0 in (b, (compress uf' x b))
     | None -> (x, uf))

  (** val find : t -> elt -> elt * t **)

  let find =
    find_x

  (** val find_repr : __ **)

  let find_repr =
    __

  (** val find_unchanged : __ **)

  let find_unchanged =
    __

  (** val sameclass_find_1 : __ **)

  let sameclass_find_1 =
    __

  (** val sameclass_find_2 : __ **)

  let sameclass_find_2 =
    __

  (** val sameclass_find_3 : __ **)

  let sameclass_find_3 =
    __
 end
