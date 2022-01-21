open BinNums
open BinPos
open Coqlib
open Datatypes
open Iteration
open Maps
open Mergesort

type node = positive

type graph = node list PTree.t

module PositiveOrd =
 struct
  type t = positive

  (** val leb : t -> t -> bool **)

  let leb x y =
    if plt y x then false else true
 end

module Sort = Sort(PositiveOrd)

type state = { gr : graph; wrk : (node * node list) list;
               map : positive PTree.t; next : positive }

(** val init_state : graph -> node -> state **)

let init_state g root =
  match PTree.get root g with
  | Some succs ->
    { gr = (PTree.remove root g); wrk = ((root, (Sort.sort succs)) :: []);
      map = PTree.empty; next = Coq_xH }
  | None -> { gr = g; wrk = []; map = PTree.empty; next = Coq_xH }

(** val transition : state -> (positive PTree.t, state) sum **)

let transition s =
  match s.wrk with
  | [] -> Coq_inl s.map
  | p :: l ->
    let (x, l0) = p in
    (match l0 with
     | [] ->
       Coq_inr { gr = s.gr; wrk = l; map = (PTree.set x s.next s.map); next =
         (Pos.succ s.next) }
     | y :: succs_x ->
       (match PTree.get y s.gr with
        | Some succs_y ->
          Coq_inr { gr = (PTree.remove y s.gr); wrk = ((y,
            (Sort.sort succs_y)) :: ((x, succs_x) :: l)); map = s.map; next =
            s.next }
        | None ->
          Coq_inr { gr = s.gr; wrk = ((x, succs_x) :: l); map = s.map; next =
            s.next }))

(** val postorder : graph -> node -> positive PTree.t **)

let postorder g root =
  WfIter.iterate transition (init_state g root)
