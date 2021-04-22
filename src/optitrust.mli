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

type path
type case_dir
type case_kind
type abort_kind
type constr_access
type enum_const_dir

type paths = path list

val cNth : ?strict:bool -> int -> path

val cCond : ?strict:bool -> unit -> path

val cThen : ?strict:bool -> unit -> path

val cElse : ?strict:bool -> unit -> path

val cBody : ?strict:bool -> unit -> path

val cInit : ?strict:bool -> unit -> path

val cStep : ?strict:bool -> unit -> path

val cAppFun : ?strict:bool -> unit -> path

val cArg : ?strict:bool -> int -> path

val cName : ?strict:bool -> unit -> path

val cDirCase : ?strict:bool -> int -> case_dir -> path

val cCaseName : int -> case_dir
val cCaseBody : case_dir

val cEnumConst : ?strict:bool -> int -> enum_const_dir -> path

val cEnumConstName : enum_const_dir
val cEnumConstVal : enum_const_dir

val cList_int : ?strict:bool -> paths -> (bool list -> int list) -> path

val cList : ?strict:bool -> paths -> (bool list -> bool list) -> path

val cFirst : ?strict:bool -> paths -> path

val (>>) : paths -> paths -> path
val (>>!) : paths -> paths -> path
val (!>>) : paths -> paths -> path
val (!>>!) : paths -> paths -> path

val (<<) : paths -> paths -> path
val (<<!) : paths -> paths -> path
val (!<<) : paths -> paths -> path
val (!<<!) : paths -> paths -> path

val cInclude : ?strict:bool -> string -> path

val cStr : ?strict:bool -> ?regexp:bool -> string -> path

val cInstrSubstr : ?strict:bool -> ?exact:bool -> ?regexp:bool -> string -> path

val cFor : ?strict:bool -> ?init:(paths) -> ?cond:(paths) ->
           ?step:(paths) -> ?body:(paths) -> ?name:string-> unit -> path

val cWhile : ?strict:bool -> ?cond:(paths) -> ?body:(paths) -> unit ->
             path

val cIf : ?strict:bool -> ?cond:(paths) -> ?then_:(paths) ->
          ?else_:(paths) -> unit -> path

val cVarDef : ?strict:bool -> ?name:string -> ?exact:bool ->
              ?body:(paths) -> unit -> path

val cFun : ?strict:bool -> ?name:string -> ?exact:bool -> ?args:(paths) ->
           ?validate:(bool list -> bool) -> ?body:(paths) -> unit -> path

val cTopFun : ?name:string -> ?exact:bool -> ?args:(paths) ->
           ?validate:(bool list -> bool) -> ?body:(paths) -> unit -> path

val cType : ?strict:bool -> ?name:string -> ?exact:bool -> unit -> path

val cEnum : ?strict:bool -> ?name:string -> ?exact:bool ->
            ?constants:((string * (paths)) list) -> unit -> path

val cSeq : ?strict:bool -> ?args:(paths) -> ?validate:(bool list -> bool) ->
           unit -> path

val cVar : ?strict:bool -> ?name:string -> ?exact:bool -> unit -> path

val cBool : ?strict:bool -> bool -> path

val cInt : ?strict:bool -> int -> path

val cDouble : ?strict:bool -> float -> path

val cString : ?strict:bool -> string -> path

(* val cPrim : ?strict:bool -> prim -> path *)

val cApp : ?strict:bool -> ?name:string -> ?fun_:(paths) ->
           ?args:(paths) -> ?validate:(bool list -> bool) -> unit -> path

val cLabel : ?strict:bool -> ?label:string -> ?exact:bool ->
             ?body:(paths) -> unit -> path

val cGoto : ?strict:bool -> ?label:string -> ?exact:bool -> unit -> path

val cReturn : ?strict:bool -> ?res:(paths) -> unit -> path

val cAbort : ?strict:bool -> ?kind:abort_kind -> unit -> path

val cAbrtAny : abort_kind
val cAbrtRet : abort_kind
val cAbrtBrk : abort_kind
val cAbrtCtn : abort_kind

val cAccesses : ?strict:bool -> ?base:(paths) ->
                ?accesses:(constr_access list) -> unit -> path

val cIndex : ?index:(paths) -> unit -> constr_access
val cField : ?field:string -> ?exact:bool -> unit -> constr_access
val cAccess : constr_access

val cSwitch : ?strict:bool -> ?cond:(paths) ->
              ?cases:((case_kind * (paths)) list) -> unit -> path

val cCase : ?value:(paths) -> unit -> case_kind
val cDefault : case_kind

val cSet : ?strict:bool -> ?lhs:(paths) -> ?rhs:(paths) -> unit -> path

(** Transformations *)

val add_label : ?replace_top:bool -> string -> paths -> unit

val show_path : ?debug_ast:bool -> ?replace_top:bool -> ?keep_previous:bool -> paths -> unit

val show_ast : ?replace_top:bool -> ?file:string -> ?to_stdout:bool -> paths -> unit

val clean_path_decorators : unit -> unit

val delete_label : ?replace_top:bool -> string -> unit

val delete_labels : ?replace_top:bool -> string list -> unit

val swap_coordinates : ?replace_top:bool -> ?name:(string -> string) ->
                       string -> unit

val split_sequence : ?replace_top:bool -> ?keep_labels:bool ->
                     ?labels:(string list) -> ?split_name:(string -> string) ->
                     paths -> unit

val extract_loop_var : ?replace_top:bool -> ?keep_label:bool -> ?label:string ->
                       paths -> unit

val extract_loop_vars : ?replace_top:bool -> ?keep_label:bool ->
                        ?label:string -> paths -> unit

val split_loop_nodep : ?replace_top:bool -> ?keep_labels:bool ->
                       ?labels:(string list) -> paths -> unit

val split_loop : ?replace_top:bool -> ?keep_labels:bool ->
                 ?labels:(string list) -> ?split_name:(string -> string) ->
                 paths -> unit

val tile_array : ?replace_top:bool -> ?name:(string -> string) ->
                 ?block_name:string -> block_size:string -> string -> unit

val fold_decl : ?replace_top:bool -> ?as_reference:bool ->
                ?fold_at:(paths list) -> decl_path:(paths) -> unit ->
                unit

val insert_decl : ?replace_top:bool -> ?insert_before:(paths) ->
                  ?insert_after:(paths) -> ?const:bool ->
                  ?as_reference:bool -> name:string -> value:string -> unit ->
                  unit

val insert_const : ?replace_top:bool -> ?insert_before:(paths) ->
                   ?insert_after:(paths) -> name:string -> value:string ->
                   unit -> unit

val insert_and_fold : ?replace_top:bool -> ?insert_before:(paths) ->
                      ?insert_after:(paths) -> ?const:bool ->
                      ?as_reference:bool -> ?fold_at:(paths list) ->
                      name:string -> value:string -> unit -> unit

val insert_typedef : ?replace_top:bool -> ?insert_before:(paths) ->
                     ?insert_after:(paths) -> name:string -> value:string ->
                     unit -> unit

val insert_and_fold_typedef : ?replace_top:bool -> ?insert_before:(paths) ->
                              ?insert_after:(paths) ->
                              ?fold_at:(paths list) -> name:string ->
                              value:string -> unit -> unit

val remove_decl : ?replace_top:bool -> decl_path:(paths) -> unit -> unit

val inline_decl : ?replace_top:bool -> ?delete_decl:bool ->
                  ?inline_at:(paths list) -> ?fun_result:string -> ?fun_args:(string list) ->
                  ?fun_return_label:string -> decl_path:(paths) -> unit ->unit

val inline_struct : ?replace_top:bool -> ?struct_name:string -> ?struct_fields:string list -> unit -> unit

val inline_record_access : ?replace_top:bool -> ?field:string -> ?var:string -> unit -> unit 

val make_explicit_record_assignment : ?replace_top:bool -> ?struct_name:string -> paths -> unit 

val make_implicit_record_assignment : ?replace_top:bool -> ?struct_name:string -> paths -> unit 

val create_subsequence : ?replace_top:bool -> ?start:paths -> ?stop:paths -> ?stop_before:bool -> ?stop_after:bool -> ?label:string -> ?braces:bool -> unit -> unit 

val array_to_variables : ?replace_top:bool -> paths -> string list -> unit 

val local_other_name : ?replace_top:bool -> ?section_of_interest:string -> ?new_var_type:string -> ?old_var:string -> ?new_var:string -> unit -> unit

val delocalize : ?replace_top:bool -> ?section_of_interest:string -> ?array_size:string -> ?neutral_element:int -> ?fold_operation:string -> unit -> unit 

val const_non_const: ?replace_top:bool -> paths -> unit 

val detach_expression : ?replace_top:bool -> ?label:string -> ?keep_label:bool->  paths -> unit 

val fields_reorder : ?replace_top:bool -> paths -> ?struct_fields:Ast.fields -> ?move_before:string -> ?move_after:string -> unit -> unit

val tile_loop : ?replace_top:bool -> paths -> unit

val loop_coloring : ?replace_top:bool -> paths -> string -> string -> unit

val loop_tile : ?replace_top:bool -> paths -> string -> string -> unit

val loop_swap : ?replace_top:bool -> paths -> unit

val move_loop_before : ?replace_top:bool -> paths -> string -> unit

val move_loop_after : ?replace_top:bool -> paths -> string -> unit

val move_loop : ?replace_top:bool -> ?move_before:string -> ?move_after:string -> string-> unit 

val aos_to_soa : ?replace_top:bool -> ?name:(string -> string) -> string -> unit

val eliminate_goto_next : ?replace_top:bool -> unit -> unit

val group_decl_init : ?replace_top:bool -> unit -> unit

val inline_seq : ?replace_top:bool -> seq_path:(paths) -> unit -> unit

val add_attribute : ?replace_top:bool -> string -> paths -> unit
