(** Context management *)

val run : (unit -> unit) -> unit

val run_unit_test : ?ast_decode:bool -> (unit -> unit) -> unit

val reset : unit -> unit

val exit_script : unit -> unit

val dump : ?out_prefix:string -> unit -> unit

val switch : ?only_branch:int -> (unit -> unit) list -> unit

val set_init_source : string -> unit

val set_repeat_io : bool -> unit

(** Path constructors *)
type constr
type target = constr list
type case_dir
type case_kind
type abort_kind
type constr_access
type enum_const_dir
type target_list_pred

val cTrue : constr

val cFalse : constr

val cBefore : constr

val cAfter : constr

val cFirst : constr

val cLast : constr

val cMulti : constr

val cAnyNb : constr

val cNb : int -> constr

val cNth : int -> constr

val cCond : constr

val cThen : constr

val cElse : constr

val cBody : constr

val cInit : constr

val cStep : constr

val cCallFun : constr

val cArg : int -> constr

val cName : constr

val cStrict : constr

val cDirCase : int -> case_dir -> constr

val cCaseName : int -> case_dir

val cCaseBody : case_dir

val cEnumConst : int -> enum_const_dir -> constr

val cEnumConstName : enum_const_dir
val cEnumConstVal : enum_const_dir

(* val cList : target -> (bool list -> bool list) -> target *)

val cInclude : string -> constr

(* val cStr : ?regexp:bool -> string -> constr *)

val cInstrSubstr : ?exact:bool -> ?regexp:bool -> string -> constr

val cStr : string -> constr 

val cStrFull : string -> constr 

val cFor : ?init:(target) -> ?cond:(target) ->
           ?step:(target) -> ?body:(target) -> string -> constr

val cWhile : ?cond:(target) -> ?body:(target) -> unit ->
             constr

val cIf : ?cond:(target) -> ?then_:(target) ->
          ?else_:(target) -> unit -> constr

val cVarDef : ?exact:bool ->
              ?body:(target) -> string -> constr

val cFunDef : ?args:(target) -> ?args_pred:target_list_pred -> ?body:(target) -> string -> constr

val cTopFun : ?args:(target) -> ?args_pred:target_list_pred -> ?body:(target) -> string -> constr

val cTypDef : ?exact:bool -> string -> constr

val cEnum : ?name:string -> ?exact:bool ->
            ?constants:((string * (target)) list) -> unit -> constr
val cSeq : ?args:(target) -> ?args_pred:target_list_pred -> unit -> constr

val cVar : ?exact:bool -> string -> constr

val cBool : bool -> constr

val cInt : int -> constr

val cDouble : float -> constr

val cString : string -> constr

val cCall : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> string -> constr

val cFun : ?fun_:target -> ?args:target -> ?args_pred:target_list_pred -> string -> constr

val cLabel : ?exact:bool -> ?body:(target) -> string -> constr

val cGoto : ?label:string -> ?exact:bool -> unit -> constr

val cReturn_target : ?res:(target) -> unit -> constr

val cReturn : constr 

val cAbort : ?kind:abort_kind -> unit -> constr

val cBreak : constr

val cContinue : constr 

val cAccesses : ?base:(target) ->
                ?accesses:(constr_access list) -> unit -> constr

val cIndex : ?index:(target) -> unit -> constr_access

val cField : ?field:string -> ?exact:bool -> unit -> constr_access

val cAccess : constr_access

val cSwitch : ?cond:(target) ->
              ?cases:((case_kind * (target)) list) -> unit -> constr

val cCase : ?value:(target) -> unit -> case_kind

val cDefault : case_kind

val make_target_list_pred : (int -> constr) -> (bool list -> bool) -> (unit -> string) -> target_list_pred

(* val cSet : ?lhs:(target) -> ?rhs:(target) -> unit -> target *)

(** Transformations *)

val add_label : ?replace_top:bool -> string -> target -> unit

val show_target : ?debug_ast:bool -> ?replace_top:bool -> ?keep_previous:bool -> target -> unit

val show_ast : ?replace_top:bool -> ?file:string -> ?to_stdout:bool -> target -> unit

val clean_target_decorators : unit -> unit

val delete_label : ?replace_top:bool -> string -> unit

val delete_labels : ?replace_top:bool -> string list -> unit

val swap_coordinates : ?replace_top:bool -> ?name:(string -> string) ->
                       string -> unit

val split_sequence : ?replace_top:bool -> ?keep_labels:bool ->
                     ?labels:(string list) -> ?split_name:(string -> string) ->
                     target -> unit

val extract_loop_var : ?replace_top:bool -> ?keep_label:bool -> ?label:string ->
                       target -> unit

val extract_loop_vars : ?replace_top:bool -> ?keep_label:bool ->
                        ?label:string -> target -> unit

val split_loop_nodep : ?replace_top:bool -> ?keep_labels:bool ->
                       ?labels:(string list) -> target -> unit

val split_loop : ?replace_top:bool -> ?keep_labels:bool ->
                 ?labels:(string list) -> ?split_name:(string -> string) ->
                 target -> unit

val tile_array : ?replace_top:bool -> ?name:(string -> string) ->
                 ?block_name:string -> block_size:string -> string -> unit

val fold_decl : ?replace_top:bool -> ?as_reference:bool ->
                ?fold_at:(target list) -> decl_target:(target) -> unit -> unit

val insert_decl : ?replace_top:bool -> ?insert_before:(target) ->
                  ?insert_after:(target) -> ?const:bool ->
                  ?as_reference:bool -> name:string -> value:string -> unit ->
                  unit

val insert_const : ?replace_top:bool -> ?insert_before:(target) ->
                   ?insert_after:(target) -> name:string -> value:string ->
                   unit -> unit

val insert_and_fold : ?replace_top:bool -> ?insert_before:(target) ->
                      ?insert_after:(target) -> ?const:bool ->
                      ?as_reference:bool -> ?fold_at:(target list) ->
                      name:string -> value:string -> unit -> unit

val insert_typedef : ?replace_top:bool -> ?insert_before:(target) ->
                     ?insert_after:(target) -> name:string -> value:string ->
                     unit -> unit

val insert_and_fold_typedef : ?replace_top:bool -> ?insert_before:(target) ->
                              ?insert_after:(target) ->
                              ?fold_at:(target list) -> name:string ->
                              value:string -> unit -> unit

val remove_decl : ?replace_top:bool -> decl_target:(target) -> unit -> unit

val inline_decl : ?replace_top:bool -> ?delete_decl:bool ->
                  ?inline_at:(target list) -> ?fun_result:string -> ?fun_args:(string list) ->
                  ?fun_return_label:string -> decl_target:(target) -> unit ->unit

val inline_struct : ?replace_top:bool -> ?struct_name:string -> ?struct_fields:string list -> unit -> unit

(* val inline_record_access : ?replace_top:bool -> ?field:string -> ?var:string -> unit -> unit  *)

val make_explicit_record_assignment : ?replace_top:bool -> ?struct_name:string -> target -> unit

val make_implicit_record_assignment : ?replace_top:bool -> ?struct_name:string -> target -> unit

val create_subsequence : ?replace_top:bool -> ?start:target -> ?stop:target -> ?stop_before:bool -> ?stop_after:bool -> ?label:string -> ?braces:bool -> unit -> unit

val array_to_variables : ?replace_top:bool -> target -> string list -> unit

val local_other_name : ?replace_top:bool -> ?section_of_interest:string -> ?new_var_type:string -> ?old_var:string -> ?new_var:string -> unit -> unit

val delocalize : ?replace_top:bool -> ?section_of_interest:string -> ?array_size:string -> ?neutral_element:int -> ?fold_operation:string -> unit -> unit

val const_non_const: ?replace_top:bool -> target -> unit

val detach_expression : ?replace_top:bool -> ?label:string -> ?keep_label:bool->  target -> unit

val remove_instruction : ?replace_top:bool -> target -> unit

val remove_instructions : ?replace_top:bool -> target list -> unit

val undetach_expression : ?replace_top:bool -> target -> unit

val fields_reorder : ?replace_top:bool -> target -> ?struct_fields:Ast.fields -> ?move_before:string -> ?move_after:string -> unit -> unit

val tile_loop : ?replace_top:bool -> target -> unit

val loop_coloring : ?replace_top:bool -> target -> string -> string -> unit

val loop_tile : ?replace_top:bool -> target -> string -> string -> unit

val loop_swap : ?replace_top:bool -> target -> unit

val move_loop_before : ?replace_top:bool -> target -> string -> unit

val move_loop_after : ?replace_top:bool -> target -> string -> unit

val move_loop : ?replace_top:bool -> ?move_before:string -> ?move_after:string -> string-> unit

val aos_to_soa : ?replace_top:bool -> ?name:(string -> string) -> string -> unit

val eliminate_goto_next : ?replace_top:bool -> unit -> unit

val group_decl_init : ?replace_top:bool -> unit -> unit

val inline_seq : ?replace_top:bool -> seq_target:(target) -> unit -> unit

val add_attribute : ?replace_top:bool -> string -> target -> unit
