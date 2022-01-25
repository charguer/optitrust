open AST
open BinNums
open Datatypes
open Kildall
open Lattice
open List0
open Maps
open RTL
open Registers

(** val reg_option_live : reg option -> Regset.t -> Regset.t **)

let reg_option_live or0 lv =
  match or0 with
  | Some r -> Regset.add r lv
  | None -> lv

(** val reg_sum_live : (reg, ident) sum -> Regset.t -> Regset.t **)

let reg_sum_live ros lv =
  match ros with
  | Coq_inl r -> Regset.add r lv
  | Coq_inr _ -> lv

(** val reg_list_live : reg list -> Regset.t -> Regset.t **)

let rec reg_list_live rl lv =
  match rl with
  | [] -> lv
  | r1 :: rs -> reg_list_live rs (Regset.add r1 lv)

(** val reg_list_dead : reg list -> Regset.t -> Regset.t **)

let rec reg_list_dead rl lv =
  match rl with
  | [] -> lv
  | r1 :: rs -> reg_list_dead rs (Regset.remove r1 lv)

(** val transfer : coq_function -> node -> Regset.t -> Regset.t **)

let transfer f pc after =
  match PTree.get pc f.fn_code with
  | Some i ->
    (match i with
     | Inop _ -> after
     | Iop (_, args, res, _) ->
       if Regset.mem res after
       then reg_list_live args (Regset.remove res after)
       else after
     | Iload (_, _, args, dst, _) ->
       if Regset.mem dst after
       then reg_list_live args (Regset.remove dst after)
       else after
     | Istore (_, _, args, src, _) ->
       reg_list_live args (Regset.add src after)
     | Icall (_, ros, args, res, _) ->
       reg_list_live args (reg_sum_live ros (Regset.remove res after))
     | Itailcall (_, ros, args) ->
       reg_list_live args (reg_sum_live ros Regset.empty)
     | Ibuiltin (_, args, res, _) ->
       reg_list_live (params_of_builtin_args args)
         (reg_list_dead (params_of_builtin_res res) after)
     | Icond (_, args, _, _) -> reg_list_live args after
     | Ijumptable (arg, _) -> Regset.add arg after
     | Ireturn optarg -> reg_option_live optarg Regset.empty)
  | None -> Regset.empty

module RegsetLat = LFSet(Regset)

module DS = Backward_Dataflow_Solver(RegsetLat)(NodeSetBackward)

(** val analyze : coq_function -> Regset.t PMap.t option **)

let analyze f =
  DS.fixpoint f.fn_code successors_instr (transfer f)

(** val last_uses_at : Regset.t PMap.t -> node -> instruction -> reg list **)

let last_uses_at live pc i =
  let l = PMap.get pc live in
  let lu = filter (fun r -> negb (Regset.mem r l)) (instr_uses i) in
  (match instr_defs i with
   | Some r -> if Regset.mem r l then lu else r :: lu
   | None -> lu)

(** val last_uses : coq_function -> reg list PTree.t **)

let last_uses f =
  match analyze f with
  | Some live -> PTree.map (last_uses_at live) f.fn_code
  | None -> PTree.empty
