(** Context management *)

val run : (unit -> unit) -> unit

val run_unit_test : ?ast_decode:bool -> (unit -> unit) -> unit

val reset : unit -> unit

val exit_script : unit -> unit

val dump : ?out_prefix:string -> unit -> unit

val switch : ?only_branch:int -> (unit -> unit) list -> unit

val set_init_source : string -> unit

(** Path constructors *)

type path
type case_dir
type case_kind
type abort_kind
type constr_access
type enum_const_dir

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

val cList_int : ?strict:bool -> path list -> (bool list -> int list) -> path

val cList : ?strict:bool -> path list -> (bool list -> bool list) -> path

val cFirst : ?strict:bool -> path list -> path

val (>>) : path list -> path list -> path
val (>>!) : path list -> path list -> path
val (!>>) : path list -> path list -> path
val (!>>!) : path list -> path list -> path

val (<<) : path list -> path list -> path
val (<<!) : path list -> path list -> path
val (!<<) : path list -> path list -> path
val (!<<!) : path list -> path list -> path

val cInclude : ?strict:bool -> string -> path

val cStr : ?strict:bool -> ?regexp:bool -> string -> path

val cInstrSubstr : ?strict:bool -> ?exact:bool -> ?regexp:bool -> string -> path

val cFor : ?strict:bool -> ?init:(path list) -> ?cond:(path list) ->
           ?step:(path list) -> ?body:(path list) -> ?name:string-> unit -> path

val cWhile : ?strict:bool -> ?cond:(path list) -> ?body:(path list) -> unit ->
             path

val cIf : ?strict:bool -> ?cond:(path list) -> ?then_:(path list) ->
          ?else_:(path list) -> unit -> path

val cVarDef : ?strict:bool -> ?name:string -> ?exact:bool ->
              ?body:(path list) -> unit -> path

val cFun : ?strict:bool -> ?name:string -> ?exact:bool -> ?args:(path list) ->
           ?validate:(bool list -> bool) -> ?body:(path list) -> unit -> path

val cTopFun : ?name:string -> ?exact:bool -> ?args:(path list) ->
           ?validate:(bool list -> bool) -> ?body:(path list) -> unit -> path

val cType : ?strict:bool -> ?name:string -> ?exact:bool -> unit -> path

val cEnum : ?strict:bool -> ?name:string -> ?exact:bool ->
            ?constants:((string * (path list)) list) -> unit -> path

val cSeq : ?strict:bool -> ?args:(path list) -> ?validate:(bool list -> bool) ->
           unit -> path

val cVar : ?strict:bool -> ?name:string -> ?exact:bool -> unit -> path

val cBool : ?strict:bool -> bool -> path

val cInt : ?strict:bool -> int -> path

val cDouble : ?strict:bool -> float -> path

val cString : ?strict:bool -> string -> path

(* val cPrim : ?strict:bool -> prim -> path *)

val cApp : ?strict:bool -> ?name:string -> ?fun_:(path list) ->
           ?args:(path list) -> ?validate:(bool list -> bool) -> unit -> path

val cLabel : ?strict:bool -> ?label:string -> ?exact:bool ->
             ?body:(path list) -> unit -> path

val cGoto : ?strict:bool -> ?label:string -> ?exact:bool -> unit -> path

val cReturn : ?strict:bool -> ?res:(path list) -> unit -> path

val cAbort : ?strict:bool -> ?kind:abort_kind -> unit -> path

val cAbrtAny : abort_kind
val cAbrtRet : abort_kind
val cAbrtBrk : abort_kind
val cAbrtCtn : abort_kind

val cAccesses : ?strict:bool -> ?base:(path list) ->
                ?accesses:(constr_access list) -> unit -> path

val cIndex : ?index:(path list) -> unit -> constr_access
val cField : ?field:string -> ?exact:bool -> unit -> constr_access
val cAccess : constr_access

val cSwitch : ?strict:bool -> ?cond:(path list) ->
              ?cases:((case_kind * (path list)) list) -> unit -> path

val cCase : ?value:(path list) -> unit -> case_kind
val cDefault : case_kind

val cSet : ?strict:bool -> ?lhs:(path list) -> ?rhs:(path list) -> unit -> path

(** Transformations *)

val add_label : ?replace_top:bool -> string -> path list -> unit

val show_path : ?debug_ast:bool -> ?replace_top:bool -> ?keep_previous:bool -> path list -> unit

val clean_path_decorators : unit -> unit

val delete_label : ?replace_top:bool -> string -> unit

val delete_labels : ?replace_top:bool -> string list -> unit

val swap_coordinates : ?replace_top:bool -> ?name:(string -> string) ->
                       string -> unit

val split_sequence : ?replace_top:bool -> ?keep_labels:bool ->
                     ?labels:(string list) -> ?split_name:(string -> string) ->
                     path list -> unit

val extract_loop_var : ?replace_top:bool -> ?keep_label:bool -> ?label:string ->
                       path list -> unit

val extract_loop_vars : ?replace_top:bool -> ?keep_label:bool ->
                        ?label:string -> path list -> unit

val split_loop_nodep : ?replace_top:bool -> ?keep_labels:bool ->
                       ?labels:(string list) -> path list -> unit

val split_loop : ?replace_top:bool -> ?keep_labels:bool ->
                 ?labels:(string list) -> ?split_name:(string -> string) ->
                 path list -> unit

val tile_array : ?replace_top:bool -> ?name:(string -> string) ->
                 ?block_name:string -> block_size:string -> string -> unit

val fold_decl : ?replace_top:bool -> ?as_reference:bool ->
                ?fold_at:(path list list) -> decl_path:(path list) -> unit ->
                unit

val insert_decl : ?replace_top:bool -> ?insert_before:(path list) ->
                  ?insert_after:(path list) -> ?const:bool ->
                  ?as_reference:bool -> name:string -> value:string -> unit ->
                  unit

val insert_const : ?replace_top:bool -> ?insert_before:(path list) ->
                   ?insert_after:(path list) -> name:string -> value:string ->
                   unit -> unit

val insert_and_fold : ?replace_top:bool -> ?insert_before:(path list) ->
                      ?insert_after:(path list) -> ?const:bool ->
                      ?as_reference:bool -> ?fold_at:(path list list) ->
                      name:string -> value:string -> unit -> unit

val insert_typedef : ?replace_top:bool -> ?insert_before:(path list) ->
                     ?insert_after:(path list) -> name:string -> value:string ->
                     unit -> unit

val insert_and_fold_typedef : ?replace_top:bool -> ?insert_before:(path list) ->
                              ?insert_after:(path list) ->
                              ?fold_at:(path list list) -> name:string ->
                              value:string -> unit -> unit

val remove_decl : ?replace_top:bool -> decl_path:(path list) -> unit -> unit

val inline_decl : ?replace_top:bool -> ?delete_decl:bool ->
                  ?inline_at:(path list list) -> ?fun_result:string ->
                  ?fun_return_label:string -> decl_path:(path list) -> unit ->unit

val inline_struct : ?replace_top:bool -> ?struct_name:string -> ?struct_fields:string list -> unit -> unit

val inline_record_access : ?replace_top:bool -> ?field:string -> ?var:string -> unit -> unit 

val make_explicit_record_assignment : ?replace_top:bool -> ?struct_name:string -> path list -> unit 

val make_implicit_record_assignment : ?replace_top:bool -> ?struct_name:string -> path list -> unit 

val create_subsequence : ?replace_top:bool -> ?braces:bool -> path list -> path list -> unit 

val array_to_variables : ?replace_top:bool -> path list -> string list -> unit 

val detach_expression : ?replace_top:bool -> ?label:string -> ?keep_label:bool->  path list -> unit 

val fields_reorder : ?replace_top:bool -> path list -> ?struct_fields:Ast.fields -> ?move_before:string -> ?move_after:string -> unit -> unit

val tile_loop : ?replace_top:bool -> path list -> unit

val loop_coloring : ?replace_top:bool -> path list -> string -> string -> unit

val loop_tile : ?replace_top:bool -> path list -> string -> string -> unit

val loop_swap : ?replace_top:bool -> path list -> unit

val move_loop_before : ?replace_top:bool -> path list -> string -> unit

val move_loop_after : ?replace_top:bool -> path list -> string -> unit

val move_loop : ?replace_top:bool -> ?move_before:string -> ?move_after:string -> string-> unit 

val aos_to_soa : ?replace_top:bool -> ?name:(string -> string) -> string -> unit

val eliminate_goto_next : ?replace_top:bool -> unit -> unit

val group_decl_init : ?replace_top:bool -> unit -> unit

val inline_seq : ?replace_top:bool -> seq_path:(path list) -> unit -> unit

val add_attribute : ?replace_top:bool -> string -> path list -> unit
