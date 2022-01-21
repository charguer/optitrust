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

val store_zeros : Mem.mem -> block -> coq_Z -> coq_Z -> Mem.mem option

module Senv :
 sig
  type t = { find_symbol : (ident -> block option);
             public_symbol : (ident -> bool);
             invert_symbol : (block -> ident option);
             block_is_volatile : (block -> bool); nextblock : block }

  val invert_symbol : t -> block -> ident option
 end

module Genv :
 sig
  type ('f, 'v) t = { genv_public : ident list; genv_symb : block PTree.t;
                      genv_defs : ('f, 'v) globdef PTree.t; genv_next : 
                      block }

  val genv_public : ('a1, 'a2) t -> ident list

  val genv_symb : ('a1, 'a2) t -> block PTree.t

  val genv_defs : ('a1, 'a2) t -> ('a1, 'a2) globdef PTree.t

  val genv_next : ('a1, 'a2) t -> block

  val find_symbol : ('a1, 'a2) t -> ident -> block option

  val public_symbol : ('a1, 'a2) t -> ident -> bool

  val find_def : ('a1, 'a2) t -> block -> ('a1, 'a2) globdef option

  val find_funct_ptr : ('a1, 'a2) t -> block -> 'a1 option

  val find_funct : ('a1, 'a2) t -> coq_val -> 'a1 option

  val invert_symbol : ('a1, 'a2) t -> block -> ident option

  val find_var_info : ('a1, 'a2) t -> block -> 'a2 globvar option

  val block_is_volatile : ('a1, 'a2) t -> block -> bool

  val add_global :
    ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) -> ('a1, 'a2) t

  val add_globals :
    ('a1, 'a2) t -> (ident * ('a1, 'a2) globdef) list -> ('a1, 'a2) t

  val empty_genv : ident list -> ('a1, 'a2) t

  val globalenv : ('a1, 'a2) program -> ('a1, 'a2) t

  val to_senv : ('a1, 'a2) t -> Senv.t

  val store_init_data :
    ('a1, 'a2) t -> Mem.mem -> block -> coq_Z -> init_data -> Mem.mem option

  val store_init_data_list :
    ('a1, 'a2) t -> Mem.mem -> block -> coq_Z -> init_data list -> Mem.mem
    option

  val perm_globvar : 'a1 globvar -> permission

  val alloc_global :
    ('a1, 'a2) t -> Mem.mem -> (ident * ('a1, 'a2) globdef) -> Mem.mem option

  val alloc_globals :
    ('a1, 'a2) t -> Mem.mem -> (ident * ('a1, 'a2) globdef) list -> Mem.mem
    option

  val init_mem : ('a1, 'a2) program -> Mem.mem option
 end
