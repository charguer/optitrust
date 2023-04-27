open AST
open BinInt
open BinNums
open BinPos
open Coqlib
open Integers
open List0
open Maps
open Memory
open Memtype
open Values

(** val store_zeros :
    Mem.mem -> block -> coq_Z -> coq_Z -> Mem.mem option **)

let rec store_zeros m b p n =
  if zle n Z0
  then Some m
  else (match Mem.store Mint8unsigned m b p coq_Vzero with
        | Some m' ->
          store_zeros m' b (Z.add p (Zpos Coq_xH)) (Z.sub n (Zpos Coq_xH))
        | None -> None)

module Senv =
 struct
  type t = { find_symbol : (ident -> block option);
             public_symbol : (ident -> bool);
             invert_symbol : (block -> ident option);
             block_is_volatile : (block -> bool); nextblock : block }

  (** val invert_symbol : t -> block -> ident option **)

  let invert_symbol t0 =
    t0.invert_symbol
 end

module Genv =
 struct
  type ('f, 'v) t = { genv_public : ident list; genv_symb : block PTree.t;
                      genv_defs : ('f, 'v) globdef PTree.t; genv_next :
                      block }

  (** val genv_public : ('a1, 'a2) t -> ident list **)

  let genv_public t0 =
    t0.genv_public

  (** val genv_symb : ('a1, 'a2) t -> block PTree.t **)

  let genv_symb t0 =
    t0.genv_symb

  (** val genv_defs : ('a1, 'a2) t -> ('a1, 'a2) globdef PTree.t **)

  let genv_defs t0 =
    t0.genv_defs

  (** val genv_next : ('a1, 'a2) t -> block **)

  let genv_next t0 =
    t0.genv_next

  (** val find_symbol : ('a1, 'a2) t -> ident -> block option **)

  let find_symbol ge id =
    PTree.get id ge.genv_symb

  (** val public_symbol : ('a1, 'a2) t -> ident -> bool **)

  let public_symbol ge id =
    match find_symbol ge id with
    | Some _ -> (fun x -> x) (in_dec ident_eq id ge.genv_public)
    | None -> false

  (** val find_def : ('a1, 'a2) t -> block -> ('a1, 'a2) globdef option **)

  let find_def ge b =
    PTree.get b ge.genv_defs

  (** val find_funct_ptr : ('a1, 'a2) t -> block -> 'a1 option **)

  let find_funct_ptr ge b =
    match find_def ge b with
    | Some g -> (match g with
                 | Gfun f -> Some f
                 | Gvar _ -> None)
    | None -> None

  (** val find_funct : ('a1, 'a2) t -> coq_val -> 'a1 option **)

  let find_funct ge = function
  | Vptr (b, ofs) ->
    if Ptrofs.eq_dec ofs Ptrofs.zero then find_funct_ptr ge b else None
  | _ -> None

  (** val invert_symbol : ('a1, 'a2) t -> block -> ident option **)

  let invert_symbol ge b =
    PTree.fold (fun res id b' -> if eq_block b b' then Some id else res)
      ge.genv_symb None

  (** val find_var_info : ('a1, 'a2) t -> block -> 'a2 globvar option **)

  let find_var_info ge b =
    match find_def ge b with
    | Some g -> (match g with
                 | Gfun _ -> None
                 | Gvar v -> Some v)
    | None -> None

  (** val block_is_volatile : ('a1, 'a2) t -> block -> bool **)

  let block_is_volatile ge b =
    match find_var_info ge b with
    | Some gv -> gv.gvar_volatile
    | None -> false

  (** val add_global :
      ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) -> ('a1, 'a2) t **)

  let add_global ge idg =
    { genv_public = ge.genv_public; genv_symb =
      (PTree.set (fst idg) ge.genv_next ge.genv_symb); genv_defs =
      (PTree.set ge.genv_next (snd idg) ge.genv_defs); genv_next =
      (Pos.succ ge.genv_next) }

  (** val add_globals :
      ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) list -> ('a1, 'a2) t **)

  let add_globals ge gl =
    fold_left add_global gl ge

  (** val empty_genv : ident list -> ('a1, 'a2) t **)

  let empty_genv pub =
    { genv_public = pub; genv_symb = PTree.empty; genv_defs = PTree.empty;
      genv_next = Coq_xH }

  (** val globalenv : ('a1, 'a2) program -> ('a1, 'a2) t **)

  let globalenv p =
    add_globals (empty_genv p.prog_public) p.prog_defs

  (** val to_senv : ('a1, 'a2) t -> Senv.t **)

  let to_senv ge =
    { Senv.find_symbol = (find_symbol ge); Senv.public_symbol =
      (public_symbol ge); Senv.invert_symbol = (invert_symbol ge);
      Senv.block_is_volatile = (block_is_volatile ge); Senv.nextblock =
      ge.genv_next }

  (** val store_init_data :
      ('a1, 'a2) t -> Mem.mem -> block -> coq_Z -> init_data -> Mem.mem option **)

  let store_init_data ge m b p = function
  | Init_int8 n -> Mem.store Mint8unsigned m b p (Vint n)
  | Init_int16 n -> Mem.store Mint16unsigned m b p (Vint n)
  | Init_int32 n -> Mem.store Mint32 m b p (Vint n)
  | Init_int64 n -> Mem.store Mint64 m b p (Vlong n)
  | Init_float32 n -> Mem.store Mfloat32 m b p (Vsingle n)
  | Init_float64 n -> Mem.store Mfloat64 m b p (Vfloat n)
  | Init_space _ -> Some m
  | Init_addrof (symb, ofs) ->
    (match find_symbol ge symb with
     | Some b' -> Mem.store coq_Mptr m b p (Vptr (b', ofs))
     | None -> None)

  (** val store_init_data_list :
      ('a1, 'a2) t -> Mem.mem -> block -> coq_Z -> init_data list -> Mem.mem
      option **)

  let rec store_init_data_list ge m b p = function
  | [] -> Some m
  | id :: idl' ->
    (match store_init_data ge m b p id with
     | Some m' ->
       store_init_data_list ge m' b (Z.add p (init_data_size id)) idl'
     | None -> None)

  (** val perm_globvar : 'a1 globvar -> permission **)

  let perm_globvar gv =
    if gv.gvar_volatile
    then Nonempty
    else if gv.gvar_readonly then Readable else Writable

  (** val alloc_global :
      ('a1, 'a2) t -> Mem.mem -> (ident * ('a1, 'a2) globdef) -> Mem.mem
      option **)

  let alloc_global ge m = function
  | (_, g) ->
    (match g with
     | Gfun _ ->
       let (m1, b) = Mem.alloc m Z0 (Zpos Coq_xH) in
       Mem.drop_perm m1 b Z0 (Zpos Coq_xH) Nonempty
     | Gvar v ->
       let init = v.gvar_init in
       let sz = init_data_list_size init in
       let (m1, b) = Mem.alloc m Z0 sz in
       (match store_zeros m1 b Z0 sz with
        | Some m2 ->
          (match store_init_data_list ge m2 b Z0 init with
           | Some m3 -> Mem.drop_perm m3 b Z0 sz (perm_globvar v)
           | None -> None)
        | None -> None))

  (** val alloc_globals :
      ('a1, 'a2) t -> Mem.mem -> (ident * ('a1, 'a2) globdef) list -> Mem.mem
      option **)

  let rec alloc_globals ge m = function
  | [] -> Some m
  | g :: gl' ->
    (match alloc_global ge m g with
     | Some m' -> alloc_globals ge m' gl'
     | None -> None)

  (** val init_mem : ('a1, 'a2) program -> Mem.mem option **)

  let init_mem p =
    alloc_globals (globalenv p) Mem.empty p.prog_defs
 end
