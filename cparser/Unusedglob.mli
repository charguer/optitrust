open AST
open BinNums
open Datatypes
open Errors
open FSetAVL
open Int0
open Iteration
open List0
open Maps
open Op
open Ordered
open RTL

module IS :
 sig
  module X' :
   sig
    type t = positive

    val eq_dec : positive -> positive -> bool

    val compare : positive -> positive -> comparison
   end

  module MSet :
   sig
    module Raw :
     sig
      type elt = positive

      type tree =
      | Leaf
      | Node of Z_as_Int.t * tree * positive * tree

      val empty : tree

      val is_empty : tree -> bool

      val mem : positive -> tree -> bool

      val min_elt : tree -> elt option

      val max_elt : tree -> elt option

      val choose : tree -> elt option

      val fold : (elt -> 'a1 -> 'a1) -> tree -> 'a1 -> 'a1

      val elements_aux : positive list -> tree -> positive list

      val elements : tree -> positive list

      val rev_elements_aux : positive list -> tree -> positive list

      val rev_elements : tree -> positive list

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
        positive -> (enumeration -> comparison) -> enumeration -> comparison

      val compare_cont :
        tree -> (enumeration -> comparison) -> enumeration -> comparison

      val compare_end : enumeration -> comparison

      val compare : tree -> tree -> comparison

      val equal : tree -> tree -> bool

      val subsetl : (tree -> bool) -> positive -> tree -> bool

      val subsetr : (tree -> bool) -> positive -> tree -> bool

      val subset : tree -> tree -> bool

      type t = tree

      val height : t -> Z_as_Int.t

      val singleton : positive -> tree

      val create : t -> positive -> t -> tree

      val assert_false : t -> positive -> t -> tree

      val bal : t -> positive -> t -> tree

      val add : positive -> tree -> tree

      val join : tree -> elt -> t -> t

      val remove_min : tree -> elt -> t -> t * elt

      val merge : tree -> tree -> tree

      val remove : positive -> tree -> tree

      val concat : tree -> tree -> tree

      type triple = { t_left : t; t_in : bool; t_right : t }

      val t_left : triple -> t

      val t_in : triple -> bool

      val t_right : triple -> t

      val split : positive -> tree -> triple

      val inter : tree -> tree -> tree

      val diff : tree -> tree -> tree

      val union : tree -> tree -> tree

      val filter : (elt -> bool) -> tree -> tree

      val partition : (elt -> bool) -> t -> t * t

      val ltb_tree : positive -> tree -> bool

      val gtb_tree : positive -> tree -> bool

      val isok : tree -> bool

      module MX :
       sig
        module OrderTac :
         sig
          module OTF :
           sig
            type t = positive

            val compare : positive -> positive -> comparison

            val eq_dec : positive -> positive -> bool
           end

          module TO :
           sig
            type t = positive

            val compare : positive -> positive -> comparison

            val eq_dec : positive -> positive -> bool
           end
         end

        val eq_dec : positive -> positive -> bool

        val lt_dec : positive -> positive -> bool

        val eqb : positive -> positive -> bool
       end

      type coq_R_min_elt =
      | R_min_elt_0 of tree
      | R_min_elt_1 of tree * Z_as_Int.t * tree * positive * tree
      | R_min_elt_2 of tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * elt option * coq_R_min_elt

      type coq_R_max_elt =
      | R_max_elt_0 of tree
      | R_max_elt_1 of tree * Z_as_Int.t * tree * positive * tree
      | R_max_elt_2 of tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * elt option * coq_R_max_elt

      module L :
       sig
        module MO :
         sig
          module OrderTac :
           sig
            module OTF :
             sig
              type t = positive

              val compare : positive -> positive -> comparison

              val eq_dec : positive -> positive -> bool
             end

            module TO :
             sig
              type t = positive

              val compare : positive -> positive -> comparison

              val eq_dec : positive -> positive -> bool
             end
           end

          val eq_dec : positive -> positive -> bool

          val lt_dec : positive -> positive -> bool

          val eqb : positive -> positive -> bool
         end
       end

      val flatten_e : enumeration -> elt list

      type coq_R_bal =
      | R_bal_0 of t * positive * t
      | R_bal_1 of t * positive * t * Z_as_Int.t * tree * positive * tree
      | R_bal_2 of t * positive * t * Z_as_Int.t * tree * positive * tree
      | R_bal_3 of t * positive * t * Z_as_Int.t * tree * positive * 
         tree * Z_as_Int.t * tree * positive * tree
      | R_bal_4 of t * positive * t
      | R_bal_5 of t * positive * t * Z_as_Int.t * tree * positive * tree
      | R_bal_6 of t * positive * t * Z_as_Int.t * tree * positive * tree
      | R_bal_7 of t * positive * t * Z_as_Int.t * tree * positive * 
         tree * Z_as_Int.t * tree * positive * tree
      | R_bal_8 of t * positive * t

      type coq_R_remove_min =
      | R_remove_min_0 of tree * elt * t
      | R_remove_min_1 of tree * elt * t * Z_as_Int.t * tree * positive
         * tree * (t * elt) * coq_R_remove_min * t * elt

      type coq_R_merge =
      | R_merge_0 of tree * tree
      | R_merge_1 of tree * tree * Z_as_Int.t * tree * positive * tree
      | R_merge_2 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * elt

      type coq_R_concat =
      | R_concat_0 of tree * tree
      | R_concat_1 of tree * tree * Z_as_Int.t * tree * positive * tree
      | R_concat_2 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * elt

      type coq_R_inter =
      | R_inter_0 of tree * tree
      | R_inter_1 of tree * tree * Z_as_Int.t * tree * positive * tree
      | R_inter_2 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * bool * t * tree
         * coq_R_inter * tree * coq_R_inter
      | R_inter_3 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * bool * t * tree
         * coq_R_inter * tree * coq_R_inter

      type coq_R_diff =
      | R_diff_0 of tree * tree
      | R_diff_1 of tree * tree * Z_as_Int.t * tree * positive * tree
      | R_diff_2 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * bool * t * tree
         * coq_R_diff * tree * coq_R_diff
      | R_diff_3 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * bool * t * tree
         * coq_R_diff * tree * coq_R_diff

      type coq_R_union =
      | R_union_0 of tree * tree
      | R_union_1 of tree * tree * Z_as_Int.t * tree * positive * tree
      | R_union_2 of tree * tree * Z_as_Int.t * tree * positive * tree
         * Z_as_Int.t * tree * positive * tree * t * bool * t * tree
         * coq_R_union * tree * coq_R_union
     end

    module E :
     sig
      type t = positive

      val compare : positive -> positive -> comparison

      val eq_dec : positive -> positive -> bool
     end

    type elt = positive

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

  type elt = positive

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
    val eqb : positive -> positive -> bool
   end

  val min_elt : t -> elt option

  val max_elt : t -> elt option

  val compare : t -> t -> t OrderedType.coq_Compare

  module E :
   sig
    type t = positive

    val compare : positive -> positive -> positive OrderedType.coq_Compare

    val eq_dec : positive -> positive -> bool
   end
 end

type workset = { w_seen : IS.t; w_todo : ident list }

val add_workset : ident -> workset -> workset

val addlist_workset : ident list -> workset -> workset

val ref_instruction : instruction -> ident list

val add_ref_instruction : workset -> node -> instruction -> workset

val add_ref_function : coq_function -> workset -> workset

val add_ref_init_data : workset -> init_data -> workset

val add_ref_globvar : unit globvar -> workset -> workset

type prog_map = (fundef, unit) globdef PTree.t

val add_ref_definition : prog_map -> ident -> workset -> workset

val initial_workset : program -> workset

val iter_step : prog_map -> workset -> (IS.t, workset) sum

val used_globals : program -> prog_map -> IS.t option

val filter_globdefs :
  IS.t -> (ident * (fundef, unit) globdef) list -> (ident * (fundef, unit)
  globdef) list -> (ident * (fundef, unit) globdef) list

val global_defined : program -> prog_map -> ident -> bool

val transform_program : program -> program res
