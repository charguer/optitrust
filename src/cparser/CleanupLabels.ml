open AST
open BinNums
open Coqlib
open Datatypes
open FSetAVL
open Int0
open Linear
open List0
open Ordered

module Labelset = Make(OrderedPositive)

(** val add_label_branched_to : Labelset.t -> instruction -> Labelset.t **)

let add_label_branched_to ls = function
| Lgoto lbl -> Labelset.add lbl ls
| Lcond (_, _, lbl) -> Labelset.add lbl ls
| Ljumptable (_, tbl) -> fold_right Labelset.add ls tbl
| _ -> ls

(** val labels_branched_to : code -> Labelset.t **)

let labels_branched_to c =
  fold_left add_label_branched_to c Labelset.empty

(** val remove_unused : Labelset.t -> instruction -> code -> code **)

let remove_unused bto i k =
  match i with
  | Llabel lbl -> if Labelset.mem lbl bto then i :: k else k
  | _ -> i :: k

(** val remove_unused_labels : Labelset.t -> code -> code **)

let remove_unused_labels bto c =
  list_fold_right (remove_unused bto) c []

(** val cleanup_labels : code -> code **)

let cleanup_labels c =
  remove_unused_labels (labels_branched_to c) c

(** val transf_function : coq_function -> coq_function **)

let transf_function f =
  { fn_sig = f.fn_sig; fn_stacksize = f.fn_stacksize; fn_code =
    (cleanup_labels f.fn_code) }

(** val transf_fundef : fundef -> fundef **)

let transf_fundef f =
  transf_fundef transf_function f

(** val transf_program : program -> program **)

let transf_program p =
  transform_program transf_fundef p
