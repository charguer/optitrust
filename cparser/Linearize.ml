open AST
open BinNums
open Coqlib
open Datatypes
open Errors
open FSetAVL
open Int0
open Kildall
open LTL
open Lattice
open Linear
open Maps
open Op
open Ordered

module DS = Dataflow_Solver(LBoolean)(NodeSetForward)

(** val reachable_aux : LTL.coq_function -> bool PMap.t option **)

let reachable_aux f =
  DS.fixpoint f.LTL.fn_code successors_block (fun _ r -> r) f.fn_entrypoint
    true

(** val reachable : LTL.coq_function -> bool PMap.t **)

let reachable f =
  match reachable_aux f with
  | Some rs -> rs
  | None -> PMap.init true

(** val enumerate_aux : LTL.coq_function -> bool PMap.t -> node list **)

let enumerate_aux = Linearizeaux.enumerate_aux

module Nodeset = Make(OrderedPositive)

(** val nodeset_of_list : node list -> Nodeset.t -> Nodeset.t res **)

let rec nodeset_of_list l s =
  match l with
  | [] -> OK s
  | hd :: tl ->
    if Nodeset.mem hd s
    then Error
           (msg
             ('L'::('i'::('n'::('e'::('a'::('r'::('i'::('z'::('e'::(':'::(' '::('d'::('u'::('p'::('l'::('i'::('c'::('a'::('t'::('e'::('s'::(' '::('i'::('n'::(' '::('e'::('n'::('u'::('m'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
    else nodeset_of_list tl (Nodeset.add hd s)

(** val check_reachable_aux :
    bool PMap.t -> Nodeset.t -> bool -> node -> bblock -> bool **)

let check_reachable_aux reach s ok pc _ =
  if PMap.get pc reach then (&&) ok (Nodeset.mem pc s) else ok

(** val check_reachable :
    LTL.coq_function -> bool PMap.t -> Nodeset.t -> bool **)

let check_reachable f reach s =
  PTree.fold (check_reachable_aux reach s) f.LTL.fn_code true

(** val enumerate : LTL.coq_function -> node list res **)

let enumerate f =
  let reach = reachable f in
  let enum = enumerate_aux f reach in
  (match nodeset_of_list enum Nodeset.empty with
   | OK x ->
     if check_reachable f reach x
     then OK enum
     else Error
            (msg
              ('L'::('i'::('n'::('e'::('a'::('r'::('i'::('z'::('e'::(':'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('e'::('n'::('u'::('m'::('e'::('r'::('a'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))
   | Error msg0 -> Error msg0)

(** val starts_with : label -> code -> bool **)

let rec starts_with lbl = function
| [] -> false
| i :: k' ->
  (match i with
   | Llabel lbl' -> if peq lbl lbl' then true else starts_with lbl k'
   | _ -> false)

(** val add_branch : label -> code -> code **)

let add_branch s k =
  if starts_with s k then k else (Lgoto s) :: k

(** val linearize_block : bblock -> code -> code **)

let rec linearize_block b k =
  match b with
  | [] -> k
  | i :: b' ->
    (match i with
     | LTL.Lop (op, args, res0) ->
       (Lop (op, args, res0)) :: (linearize_block b' k)
     | LTL.Lload (chunk, addr, args, dst) ->
       (Lload (chunk, addr, args, dst)) :: (linearize_block b' k)
     | LTL.Lgetstack (sl, ofs, ty, dst) ->
       (Lgetstack (sl, ofs, ty, dst)) :: (linearize_block b' k)
     | LTL.Lsetstack (src, sl, ofs, ty) ->
       (Lsetstack (src, sl, ofs, ty)) :: (linearize_block b' k)
     | LTL.Lstore (chunk, addr, args, src) ->
       (Lstore (chunk, addr, args, src)) :: (linearize_block b' k)
     | LTL.Lcall (sig0, ros) -> (Lcall (sig0, ros)) :: (linearize_block b' k)
     | LTL.Ltailcall (sig0, ros) -> (Ltailcall (sig0, ros)) :: k
     | LTL.Lbuiltin (ef, args, res0) ->
       (Lbuiltin (ef, args, res0)) :: (linearize_block b' k)
     | Lbranch s -> add_branch s k
     | LTL.Lcond (cond, args, s1, s2) ->
       if starts_with s1 k
       then (Lcond ((negate_condition cond), args, s2)) :: (add_branch s1 k)
       else (Lcond (cond, args, s1)) :: (add_branch s2 k)
     | LTL.Ljumptable (arg, tbl) -> (Ljumptable (arg, tbl)) :: k
     | LTL.Lreturn -> Lreturn :: k)

(** val linearize_node : LTL.coq_function -> node -> code -> code **)

let linearize_node f pc k =
  match PTree.get pc f.LTL.fn_code with
  | Some b -> (Llabel pc) :: (linearize_block b k)
  | None -> k

(** val linearize_body : LTL.coq_function -> node list -> code **)

let linearize_body f enum =
  list_fold_right (linearize_node f) enum []

(** val transf_function : LTL.coq_function -> coq_function res **)

let transf_function f =
  match enumerate f with
  | OK x ->
    OK { fn_sig = f.LTL.fn_sig; fn_stacksize = f.LTL.fn_stacksize; fn_code =
      (add_branch f.fn_entrypoint (linearize_body f x)) }
  | Error msg0 -> Error msg0

(** val transf_fundef : LTL.fundef -> fundef res **)

let transf_fundef f =
  transf_partial_fundef transf_function f

(** val transf_program : LTL.program -> program res **)

let transf_program p =
  transform_partial_program transf_fundef p
