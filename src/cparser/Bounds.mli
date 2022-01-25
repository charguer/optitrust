open AST
open BinInt
open BinNums
open Conventions
open Conventions1
open Coqlib
open Datatypes
open FSetAVL
open Int0
open Linear
open List0
open Locations
open Machregs
open Ordered

module RegOrd :
 sig
  type t = IndexedMreg.t

  val compare : t -> t -> t OrderedType.coq_Compare

  val eq_dec : t -> t -> bool
 end

module RegSet :
 sig
  module X' :
   sig
    type t = IndexedMreg.t

    val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool

    val compare : IndexedMreg.t -> IndexedMreg.t -> comparison
   end

  module MSet :
   sig
    module Raw :
     sig
      type elt = IndexedMreg.t

      type tree =
      | Leaf
      | Node of Z_as_Int.t * tree * IndexedMreg.t * tree

      val empty : tree

      val is_empty : tree -> bool

      val mem : IndexedMreg.t -> tree -> bool

      val min_elt : tree -> elt option

      val max_elt : tree -> elt option

      val choose : tree -> elt option

      val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1

      val elements_aux : IndexedMreg.t list -> tree -> IndexedMreg.t list

      val elements : tree -> IndexedMreg.t list

      val rev_elements_aux : IndexedMreg.t list -> tree -> IndexedMreg.t list

      val rev_elements : tree -> IndexedMreg.t list

      val cardinal : tree -> nat

      val maxdepth : tree -> nat

      val mindepth : tree -> nat

      val for_all : (elt -> bool) -> tree -> bool

      val exists_ : (elt -> bool) -> tree -> bool

      type enumeration =
      | End
      | More of elt * tree * enumeration

      val cons : tree -> enumeration -> enumeration

      val compare_more :
        IndexedMreg.t -> (enumeration -> comparison) -> enumeration ->
        comparison

      val compare_cont :
        tree -> (enumeration -> comparison) -> enumeration -> comparison

      val compare_end : enumeration -> comparison

      val compare : tree -> tree -> comparison

      val equal : tree -> tree -> bool

      val subsetl : (tree -> bool) -> IndexedMreg.t -> tree -> bool

      val subsetr : (tree -> bool) -> IndexedMreg.t -> tree -> bool

      val subset : tree -> tree -> bool

      type t = tree

      val height : t -> Z_as_Int.t

      val singleton : IndexedMreg.t -> tree

      val create : t -> IndexedMreg.t -> t -> tree

      val assert_false : t -> IndexedMreg.t -> t -> tree

      val bal : t -> IndexedMreg.t -> t -> tree

      val add : IndexedMreg.t -> tree -> tree

      val join : tree -> elt -> t -> t

      val remove_min : tree -> elt -> t -> t * elt

      val merge : tree -> tree -> tree

      val remove : IndexedMreg.t -> tree -> tree

      val concat : tree -> tree -> tree

      type triple = { t_left : t; t_in : bool; t_right : t }

      val t_left : triple -> t

      val t_in : triple -> bool

      val t_right : triple -> t

      val split : IndexedMreg.t -> tree -> triple

      val inter : tree -> tree -> tree

      val diff : tree -> tree -> tree

      val union : tree -> tree -> tree

      val filter : (elt -> bool) -> tree -> tree

      val partition : (elt -> bool) -> t -> t * t

      val ltb_tree : IndexedMreg.t -> tree -> bool

      val gtb_tree : IndexedMreg.t -> tree -> bool

      val isok : tree -> bool

      module MX :
       sig
        module OrderTac :
         sig
          module OTF :
           sig
            type t = IndexedMreg.t

            val compare : IndexedMreg.t -> IndexedMreg.t -> comparison

            val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool
           end

          module TO :
           sig
            type t = IndexedMreg.t

            val compare : IndexedMreg.t -> IndexedMreg.t -> comparison

            val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool
           end
         end

        val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool

        val lt_dec : IndexedMreg.t -> IndexedMreg.t -> bool

        val eqb : IndexedMreg.t -> IndexedMreg.t -> bool
       end

      type coq_R_min_elt =
      | R_min_elt_0 of tree
      | R_min_elt_1 of tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_min_elt_2 of tree * Z_as_Int.t * tree * IndexedMreg.t * tree
         * Z_as_Int.t * tree * IndexedMreg.t * tree * elt option
         * coq_R_min_elt

      type coq_R_max_elt =
      | R_max_elt_0 of tree
      | R_max_elt_1 of tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_max_elt_2 of tree * Z_as_Int.t * tree * IndexedMreg.t * tree
         * Z_as_Int.t * tree * IndexedMreg.t * tree * elt option
         * coq_R_max_elt

      module L :
       sig
        module MO :
         sig
          module OrderTac :
           sig
            module OTF :
             sig
              type t = IndexedMreg.t

              val compare : IndexedMreg.t -> IndexedMreg.t -> comparison

              val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool
             end

            module TO :
             sig
              type t = IndexedMreg.t

              val compare : IndexedMreg.t -> IndexedMreg.t -> comparison

              val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool
             end
           end

          val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool

          val lt_dec : IndexedMreg.t -> IndexedMreg.t -> bool

          val eqb : IndexedMreg.t -> IndexedMreg.t -> bool
         end
       end

      val flatten_e : enumeration -> elt list

      type coq_R_bal =
      | R_bal_0 of t * IndexedMreg.t * t
      | R_bal_1 of t * IndexedMreg.t * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree
      | R_bal_2 of t * IndexedMreg.t * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree
      | R_bal_3 of t * IndexedMreg.t * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_bal_4 of t * IndexedMreg.t * t
      | R_bal_5 of t * IndexedMreg.t * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree
      | R_bal_6 of t * IndexedMreg.t * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree
      | R_bal_7 of t * IndexedMreg.t * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_bal_8 of t * IndexedMreg.t * t

      type coq_R_remove_min =
      | R_remove_min_0 of tree * elt * t
      | R_remove_min_1 of tree * elt * t * Z_as_Int.t * tree * IndexedMreg.t
         * tree * (t * elt) * coq_R_remove_min * t * elt

      type coq_R_merge =
      | R_merge_0 of tree * tree
      | R_merge_1 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_merge_2 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * elt

      type coq_R_concat =
      | R_concat_0 of tree * tree
      | R_concat_1 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_concat_2 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * elt

      type coq_R_inter =
      | R_inter_0 of tree * tree
      | R_inter_1 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_inter_2 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * bool * 
         t * tree * coq_R_inter * tree * coq_R_inter
      | R_inter_3 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * bool * 
         t * tree * coq_R_inter * tree * coq_R_inter

      type coq_R_diff =
      | R_diff_0 of tree * tree
      | R_diff_1 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_diff_2 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * bool * 
         t * tree * coq_R_diff * tree * coq_R_diff
      | R_diff_3 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * bool * 
         t * tree * coq_R_diff * tree * coq_R_diff

      type coq_R_union =
      | R_union_0 of tree * tree
      | R_union_1 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * tree
      | R_union_2 of tree * tree * Z_as_Int.t * tree * IndexedMreg.t * 
         tree * Z_as_Int.t * tree * IndexedMreg.t * tree * t * bool * 
         t * tree * coq_R_union * tree * coq_R_union
     end

    module E :
     sig
      type t = IndexedMreg.t

      val compare : IndexedMreg.t -> IndexedMreg.t -> comparison

      val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool
     end

    type elt = IndexedMreg.t

    type t_ = Raw.t
      (* singleton inductive, whose constructor was Mkt *)

    val this : t_ -> Raw.t

    type t = t_

    val mem : elt -> t -> bool

    val add : elt -> t -> t

    val remove : elt -> t -> t

    val singleton : elt -> t

    val union : t -> t -> t

    val inter : t -> t -> t

    val diff : t -> t -> t

    val equal : t -> t -> bool

    val subset : t -> t -> bool

    val empty : t

    val is_empty : t -> bool

    val elements : t -> elt list

    val choose : t -> elt option

    val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1

    val cardinal : t -> nat

    val filter : (elt -> bool) -> t -> t

    val for_all : (elt -> bool) -> t -> bool

    val exists_ : (elt -> bool) -> t -> bool

    val partition : (elt -> bool) -> t -> t * t

    val eq_dec : t -> t -> bool

    val compare : t -> t -> comparison

    val min_elt : t -> elt option

    val max_elt : t -> elt option
   end

  type elt = IndexedMreg.t

  type t = MSet.t

  val empty : t

  val is_empty : t -> bool

  val mem : elt -> t -> bool

  val add : elt -> t -> t

  val singleton : elt -> t

  val remove : elt -> t -> t

  val union : t -> t -> t

  val inter : t -> t -> t

  val diff : t -> t -> t

  val eq_dec : t -> t -> bool

  val equal : t -> t -> bool

  val subset : t -> t -> bool

  val fold : (elt -> 'a1 -> 'a1) -> t -> 'a1 -> 'a1

  val for_all : (elt -> bool) -> t -> bool

  val exists_ : (elt -> bool) -> t -> bool

  val filter : (elt -> bool) -> t -> t

  val partition : (elt -> bool) -> t -> t * t

  val cardinal : t -> nat

  val elements : t -> elt list

  val choose : t -> elt option

  module MF :
   sig
    val eqb : IndexedMreg.t -> IndexedMreg.t -> bool
   end

  val min_elt : t -> elt option

  val max_elt : t -> elt option

  val compare : t -> t -> t OrderedType.coq_Compare

  module E :
   sig
    type t = IndexedMreg.t

    val compare :
      IndexedMreg.t -> IndexedMreg.t -> IndexedMreg.t OrderedType.coq_Compare

    val eq_dec : IndexedMreg.t -> IndexedMreg.t -> bool
   end
 end

type bounds = { used_callee_save : mreg list; bound_local : coq_Z;
                bound_outgoing : coq_Z; bound_stack_data : coq_Z }

val record_reg : RegSet.t -> mreg -> RegSet.t

val record_regs : RegSet.t -> mreg list -> RegSet.t

val record_regs_of_instr : RegSet.t -> instruction -> RegSet.t

val record_regs_of_function : coq_function -> RegSet.t

val slots_of_locs : loc list -> ((slot * coq_Z) * typ) list

val slots_of_instr : instruction -> ((slot * coq_Z) * typ) list

val max_over_list : ('a1 -> coq_Z) -> 'a1 list -> coq_Z

val max_over_instrs : coq_function -> (instruction -> coq_Z) -> coq_Z

val max_over_slots_of_instr :
  (((slot * coq_Z) * typ) -> coq_Z) -> instruction -> coq_Z

val max_over_slots_of_funct :
  coq_function -> (((slot * coq_Z) * typ) -> coq_Z) -> coq_Z

val local_slot : ((slot * coq_Z) * typ) -> coq_Z

val outgoing_slot : ((slot * coq_Z) * typ) -> coq_Z

val outgoing_space : instruction -> coq_Z

val function_bounds : coq_function -> bounds

val size_callee_save_area_rec : mreg list -> coq_Z -> coq_Z

val size_callee_save_area : bounds -> coq_Z -> coq_Z

type frame_env = { fe_size : coq_Z; fe_ofs_link : coq_Z;
                   fe_ofs_retaddr : coq_Z; fe_ofs_local : coq_Z;
                   fe_ofs_callee_save : coq_Z; fe_stack_data : coq_Z;
                   fe_used_callee_save : mreg list }
