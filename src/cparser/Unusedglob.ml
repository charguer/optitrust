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

module IS = Make(OrderedPositive)

type workset = { w_seen : IS.t; w_todo : ident list }

(** val add_workset : ident -> workset -> workset **)

let add_workset id w =
  if IS.mem id w.w_seen
  then w
  else { w_seen = (IS.add id w.w_seen); w_todo = (id :: w.w_todo) }

(** val addlist_workset : ident list -> workset -> workset **)

let rec addlist_workset l w =
  match l with
  | [] -> w
  | id :: l' -> addlist_workset l' (add_workset id w)

(** val ref_instruction : instruction -> ident list **)

let ref_instruction = function
| Iop (op, _, _, _) -> globals_operation op
| Iload (_, addr, _, _, _) -> globals_addressing addr
| Istore (_, addr, _, _, _) -> globals_addressing addr
| Icall (_, s0, _, _, _) ->
  (match s0 with
   | Coq_inl _ -> []
   | Coq_inr id -> id :: [])
| Itailcall (_, s0, _) ->
  (match s0 with
   | Coq_inl _ -> []
   | Coq_inr id -> id :: [])
| Ibuiltin (_, args, _, _) -> globals_of_builtin_args args
| _ -> []

(** val add_ref_instruction : workset -> node -> instruction -> workset **)

let add_ref_instruction w _ i =
  addlist_workset (ref_instruction i) w

(** val add_ref_function : coq_function -> workset -> workset **)

let add_ref_function f w =
  PTree.fold add_ref_instruction f.fn_code w

(** val add_ref_init_data : workset -> init_data -> workset **)

let add_ref_init_data w = function
| Init_addrof (id, _) -> add_workset id w
| _ -> w

(** val add_ref_globvar : unit globvar -> workset -> workset **)

let add_ref_globvar gv w =
  fold_left add_ref_init_data gv.gvar_init w

type prog_map = (fundef, unit) globdef PTree.t

(** val add_ref_definition : prog_map -> ident -> workset -> workset **)

let add_ref_definition pm id w =
  match PTree.get id pm with
  | Some g ->
    (match g with
     | Gfun f0 ->
       (match f0 with
        | Internal f -> add_ref_function f w
        | External _ -> w)
     | Gvar gv -> add_ref_globvar gv w)
  | None -> w

(** val initial_workset : program -> workset **)

let initial_workset p =
  add_workset p.prog_main
    (fold_left (fun w id -> add_workset id w) p.prog_public { w_seen =
      IS.empty; w_todo = [] })

(** val iter_step : prog_map -> workset -> (IS.t, workset) sum **)

let iter_step pm w =
  match w.w_todo with
  | [] -> Coq_inl w.w_seen
  | id :: rem ->
    Coq_inr (add_ref_definition pm id { w_seen = w.w_seen; w_todo = rem })

(** val used_globals : program -> prog_map -> IS.t option **)

let used_globals p pm =
  PrimIter.iterate (iter_step pm) (initial_workset p)

(** val filter_globdefs :
    IS.t -> (ident * (fundef, unit) globdef) list -> (ident * (fundef, unit)
    globdef) list -> (ident * (fundef, unit) globdef) list **)

let rec filter_globdefs used accu = function
| [] -> accu
| p :: defs' ->
  let (id, gd) = p in
  if IS.mem id used
  then filter_globdefs (IS.remove id used) ((id, gd) :: accu) defs'
  else filter_globdefs used accu defs'

(** val global_defined : program -> prog_map -> ident -> bool **)

let global_defined p pm id =
  match PTree.get id pm with
  | Some _ -> true
  | None -> (fun x -> x) (ident_eq id p.prog_main)

(** val transform_program : program -> program res **)

let transform_program p =
  let pm = prog_defmap p in
  (match used_globals p pm with
   | Some used ->
     if IS.for_all (global_defined p pm) used
     then OK { prog_defs = (filter_globdefs used [] (rev p.prog_defs));
            prog_public = p.prog_public; prog_main = p.prog_main }
     else Error
            (msg
              ('U'::('n'::('u'::('s'::('e'::('d'::('g'::('l'::('o'::('b'::(':'::(' '::('r'::('e'::('f'::('e'::('r'::('e'::('n'::('c'::('e'::(' '::('t'::('o'::(' '::('u'::('n'::('d'::('e'::('f'::('i'::('n'::('e'::('d'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::[]))))))))))))))))))))))))))))))))))))))))))
   | None ->
     Error
       (msg
         ('U'::('n'::('u'::('s'::('e'::('d'::('g'::('l'::('o'::('b'::(':'::(' '::('a'::('n'::('a'::('l'::('y'::('s'::('i'::('s'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
