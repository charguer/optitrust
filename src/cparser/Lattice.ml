open BinNums
open Bool
open FSetInterface
open Maps

module type SEMILATTICE =
 sig
  type t

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t
 end

module type SEMILATTICE_WITH_TOP =
 sig
  type t

  val beq : t -> t -> bool

  val bot : t

  val lub : t -> t -> t

  val top : t
 end

module LPMap1 =
 functor (L:SEMILATTICE) ->
 struct
  type t = L.t PTree.t

  (** val get : positive -> t -> L.t **)

  let get p x =
    match PTree.get p x with
    | Some x0 -> x0
    | None -> L.bot

  (** val set : positive -> L.t -> t -> t **)

  let set p v x =
    if L.beq v L.bot then PTree.remove p x else PTree.set p v x

  (** val beq : t -> t -> bool **)

  let beq x y =
    PTree.beq L.beq x y

  (** val bot : t **)

  let bot =
    PTree.empty

  (** val opt_beq : L.t option -> L.t option -> bool **)

  let opt_beq ox oy =
    match ox with
    | Some x -> (match oy with
                 | Some y -> L.beq x y
                 | None -> false)
    | None -> (match oy with
               | Some _ -> false
               | None -> true)

  type changed =
  | Same
  | Same1
  | Same2
  | Changed of L.t PTree.t

  (** val xcombine :
      (L.t option -> L.t option -> L.t option) -> L.t PTree.tree -> L.t
      PTree.tree -> changed **)

  let xcombine f =
    let node_combine_l = fun l1 lres o1 r1 rres ->
      let o' = f o1 None in
      (match lres with
       | Same1 ->
         (match rres with
          | Same1 ->
            if opt_beq o' o1 then Same1 else Changed (PTree.coq_Node l1 o' r1)
          | Changed r' -> Changed (PTree.coq_Node l1 o' r')
          | _ -> Same)
       | Changed l' ->
         (match rres with
          | Same1 -> Changed (PTree.coq_Node l' o' r1)
          | Changed r' -> Changed (PTree.coq_Node l' o' r')
          | _ -> Same)
       | _ -> Same)
    in
    let xcombine_l = fun m ->
      match m with
      | PTree.Empty -> Same1
      | PTree.Nodes m' -> PTree.tree_rec' Same1 node_combine_l m'
    in
    let node_combine_r = fun l1 lres o1 r1 rres ->
      let o' = f None o1 in
      (match lres with
       | Same2 ->
         (match rres with
          | Same2 ->
            if opt_beq o' o1 then Same2 else Changed (PTree.coq_Node l1 o' r1)
          | Changed r' -> Changed (PTree.coq_Node l1 o' r')
          | _ -> Same)
       | Changed l' ->
         (match rres with
          | Same2 -> Changed (PTree.coq_Node l' o' r1)
          | Changed r' -> Changed (PTree.coq_Node l' o' r')
          | _ -> Same)
       | _ -> Same)
    in
    let xcombine_r = fun m ->
      match m with
      | PTree.Empty -> Same2
      | PTree.Nodes m' -> PTree.tree_rec' Same2 node_combine_r m'
    in
    let node_combine_2 = fun l1 o1 r1 l2 o2 r2 lres rres ->
      let o = f o1 o2 in
      (match lres with
       | Same ->
         (match rres with
          | Same ->
            if opt_beq o o1
            then if opt_beq o o2 then Same else Same1
            else if opt_beq o o2
                 then Same2
                 else Changed (PTree.coq_Node l1 o r1)
          | Same1 ->
            if opt_beq o o1 then Same1 else Changed (PTree.coq_Node l1 o r1)
          | Same2 ->
            if opt_beq o o2 then Same2 else Changed (PTree.coq_Node l2 o r2)
          | Changed m2 -> Changed (PTree.coq_Node l1 o m2))
       | Same1 ->
         (match rres with
          | Same2 -> Changed (PTree.coq_Node l1 o r2)
          | Changed m2 -> Changed (PTree.coq_Node l1 o m2)
          | _ ->
            if opt_beq o o1 then Same1 else Changed (PTree.coq_Node l1 o r1))
       | Same2 ->
         (match rres with
          | Same1 -> Changed (PTree.coq_Node l2 o r1)
          | Changed m2 -> Changed (PTree.coq_Node l2 o m2)
          | _ ->
            if opt_beq o o2 then Same2 else Changed (PTree.coq_Node l2 o r2))
       | Changed m1 ->
         (match rres with
          | Same2 -> Changed (PTree.coq_Node m1 o r2)
          | Changed m2 -> Changed (PTree.coq_Node m1 o m2)
          | _ -> Changed (PTree.coq_Node m1 o r1)))
    in
    PTree.tree_rec2 Same xcombine_r xcombine_l node_combine_2

  (** val combine :
      (L.t option -> L.t option -> L.t option) -> L.t PTree.t -> L.t PTree.t
      -> L.t PTree.t **)

  let combine f m1 m2 =
    match xcombine f m1 m2 with
    | Same2 -> m2
    | Changed m -> m
    | _ -> m1

  (** val lub : t -> t -> t **)

  let lub x y =
    combine (fun a b ->
      match a with
      | Some u -> (match b with
                   | Some v -> Some (L.lub u v)
                   | None -> a)
      | None -> b) x y
 end

module LPMap =
 functor (L:SEMILATTICE_WITH_TOP) ->
 struct
  type t' =
  | Bot
  | Top_except of L.t PTree.t

  type t = t'

  (** val get : positive -> t -> L.t **)

  let get p = function
  | Bot -> L.bot
  | Top_except m -> (match PTree.get p m with
                     | Some x0 -> x0
                     | None -> L.top)

  (** val set : positive -> L.t -> t -> t **)

  let set p v = function
  | Bot -> Bot
  | Top_except m ->
    if L.beq v L.bot
    then Bot
    else Top_except
           (if L.beq v L.top then PTree.remove p m else PTree.set p v m)

  (** val beq : t -> t -> bool **)

  let beq x y =
    match x with
    | Bot -> (match y with
              | Bot -> true
              | Top_except _ -> false)
    | Top_except m ->
      (match y with
       | Bot -> false
       | Top_except n -> PTree.beq L.beq m n)

  (** val bot : t' **)

  let bot =
    Bot

  (** val top : t' **)

  let top =
    Top_except PTree.empty

  module LM = LPMap1(L)

  (** val opt_lub : L.t -> L.t -> L.t option **)

  let opt_lub x y =
    let z = L.lub x y in if L.beq z L.top then None else Some z

  (** val lub : t -> t -> t **)

  let lub x y =
    match x with
    | Bot -> y
    | Top_except m ->
      (match y with
       | Bot -> x
       | Top_except n ->
         Top_except
           (LM.combine (fun a b ->
             match a with
             | Some u -> (match b with
                          | Some v -> opt_lub u v
                          | None -> None)
             | None -> None) m n))
 end

module LFSet =
 functor (S:WS) ->
 struct
  type t = S.t

  (** val beq : t -> t -> bool **)

  let beq =
    S.equal

  (** val bot : t **)

  let bot =
    S.empty

  (** val lub : t -> t -> t **)

  let lub =
    S.union
 end

module LBoolean =
 struct
  type t = bool

  (** val beq : t -> t -> bool **)

  let beq =
    eqb

  (** val bot : bool **)

  let bot =
    false

  (** val top : bool **)

  let top =
    true

  (** val lub : t -> t -> bool **)

  let lub =
    (||)
 end
