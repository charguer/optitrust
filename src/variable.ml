open Ast
include Variable_core.Rename
include Variable_basic

(* [fold ~as_reference ~at ~nonconst tg] expects [tg] to point to a variable declaration
    [as_reference] - denotes a flag whether the declaration initialization contains a
      variable reference or not.
    [at] - denotes a list of targets where the folding is done. If empty the
      folding operation is performed on all the ast nodes in the same level as the
      declaration or deeper, by default [at] = [].
    [nonconst] - denotes a flag to decide if folding should be done for variable which are
        not mutable, in general is not safe to fold variables which are not declared as const.
        But if the user knows what he/she is doing than it can use this flag to use folding
        also for mutable variables.
    This transformation
*)
let fold ?(as_reference : bool = false) ?(at : Target.target = []) ?(nonconst : bool = false) (tg : Target.target) : unit =
  Target.iter_on_targets (fun t p ->
    let (tg_trm, _) = Path.resolve_path p t in
    match tg_trm.desc with
    | Trm_let (vk, (_, tx), _) ->
      let ty = get_inner_ptr_type tx in
      begin match ty.typ_desc with 
      (* If the declared variable has a refernce type checking its mutability is not needed*)
      | Typ_ptr {ptr_kind = Ptr_kind_ref;_} ->
        Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
      (* In other cases we need to check the mutability of the variable *)
      | _ -> begin match vk with
            | Var_immutable -> Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
            | _ -> if nonconst = true
                then Variable_basic.fold ~as_reference ~at (Target.target_of_path p)
                else
                  fail tg_trm.loc "fold: if you want to use folding for mutable variables you should set
                            ~nonconst to true when calling this transformation"
            end
      end
      
    | _ -> fail tg_trm.loc "fold: expected a variable declaration"
) tg

(* [insert_and_fold] expects [tg] to point to relative location, then it inserts a new variable declaration at that location. 
    The new declared variable is [name] with typ [typ] and value [value]. This variable will be folded everywhere on the ast nodes
    which come after the declared variable.
*)
let insert_and_fold (name : string) (typ : string) (value : string) (tg : Target.target) : unit = 
  Variable_basic.insert name typ value tg;
  Variable_basic.fold [Target.cVarDef name]

(* [local_other_name var_type old_name new_name] similar to the basic version of local_other_name but with the intermediate
      done autmatically
*)
let local_other_name ?(mark : mark = "section_of_interest") ~var_type:(vt : typ) ~old_var:(ov : var) ~new_var:(nv : var) () : unit =
  Sequence_basic.intro_on_instr ~mark  ~visible:false [Target.tIndex 0; Target.cFor ~body:[Target.cVar ov]""];  
  Variable_basic.local_other_name  ~var_type:vt ~old_var:ov ~new_var:nv [Target.cMark mark]


(* [delocalize ~var_type ~old_var ~new_var ~mark ~arr_size ~neutral_element fold_operation tg] 
    expects the target [tg] to point to a for loop. Then it will surround this loop with a @nobrace
    sequence. After that it will apply another transformation called local other name. Which as the name
    suggests it will declare a new variable inside the targeted block and replace the current one with t he new one.
    Finally a last instruction is added to save all the changes to the old variable. Now the stage is 
    ready to apply delocalize which basically declares a new array oof size [array_size]. Then it will
    transform the old loop into a parallel loop. And finally a reduction is done to save the result into
    the old variable.
*)

let delocalize ?(loop_index : string = "dl_i") ?(mark : mark = "section_of_interest") ?(dl_ops : delocalize_ops = Delocalize_arith (Lit_int 0, Binop_add) ) 
   ~old_var:(ov : var) ~new_var:(nv : var)  ~var_type:(vt : typ) 
  ~array_size:(arrs : string) () : unit =
  local_other_name ~mark ~var_type:vt ~old_var:ov ~new_var:nv ();
  Variable_basic.delocalize ~loop_index ~array_size:arrs ~dl_ops [Target.cMark mark]


let delocalize_in_vars ?(loop_index : string = "dl_i") ?(mark : mark = "section_of_interest") ?(dl_ops : delocalize_ops = Delocalize_arith (Lit_int 0, Binop_add) ) 
   ~old_var:(ov : var) ~new_var:(nv : var)  ~var_type:(vt : typ) 
  ~array_size:(arrs : string) () : unit =
  local_other_name ~mark ~var_type:vt ~old_var:ov ~new_var:nv ();
  Variable_basic.delocalize ~loop_index ~array_size:arrs ~dl_ops [Target.cMark mark];
  Variable_basic.inline_at [Target.cFor loop_index] [Target.nbAny;Target.cVarDef arrs];
  Loop_basic.unroll ~braces:false [Target.nbMulti ;Target.cFor loop_index];
  Trace.reparse()
  
