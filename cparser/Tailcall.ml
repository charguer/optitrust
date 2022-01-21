open AST
open BinNums
open Conventions
open Coqlib
open Datatypes
open Maps
open Op
open RTL
open Registers

(** val is_return : nat -> coq_function -> node -> reg -> bool **)

let rec is_return n f pc rret =
  match n with
  | O -> false
  | S n' ->
    (match PTree.get pc f.fn_code with
     | Some i ->
       (match i with
        | Inop s -> is_return n' f s rret
        | Iop (op, args, dst, s) ->
          (match is_move_operation op args with
           | Some src ->
             if Reg.eq rret src then is_return n' f s dst else false
           | None -> false)
        | Ireturn o ->
          (match o with
           | Some r -> (fun x -> x) (Reg.eq r rret)
           | None -> true)
        | _ -> false)
     | None -> false)

(** val niter : nat **)

let niter =
  S (S (S (S (S O))))

(** val transf_instr : coq_function -> node -> instruction -> instruction **)

let transf_instr f _ instr = match instr with
| Icall (sig0, ros, args, res, s) ->
  if (&&) ((&&) (is_return niter f s res) (tailcall_is_possible sig0))
       ((fun x -> x) (rettype_eq sig0.sig_res f.fn_sig.sig_res))
  then Itailcall (sig0, ros, args)
  else instr
| _ -> instr

(** val transf_function : coq_function -> coq_function **)

let transf_function f =
  if zeq f.fn_stacksize Z0 then transf_function (transf_instr f) f else f

(** val transf_fundef : fundef -> fundef **)

let transf_fundef fd =
  transf_fundef transf_function fd

(** val transf_program : program -> program **)

let transf_program p =
  transform_program transf_fundef p
