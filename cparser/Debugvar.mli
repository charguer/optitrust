open AST
open BinNums
open BinPos
open Conventions1
open Datatypes
open Errors
open Floats
open Integers
open Iteration
open Linear
open List0
open Locations
open Machregs
open Maps

type debuginfo = loc builtin_arg

val normalize_debug_1 : loc builtin_arg -> debuginfo option

val normalize_debug : loc builtin_arg list -> debuginfo option

type avail = (ident * debuginfo) list

val set_state : ident -> debuginfo -> avail -> avail

val remove_state : ident -> avail -> avail

val set_debug_info : ident -> loc builtin_arg list -> avail -> avail

val arg_no_overlap : loc builtin_arg -> loc -> bool

val kill : loc -> avail -> avail

val kill_res : mreg builtin_res -> avail -> avail

val arg_preserved : loc builtin_arg -> bool

val kill_at_call : avail -> avail

val eq_arg : loc builtin_arg -> loc builtin_arg -> bool

val eq_debuginfo : debuginfo -> debuginfo -> bool

val join : avail -> avail -> avail

val eq_state : avail -> avail -> bool

val top : avail

type labelmap = avail PTree.t * bool

val get_label : label -> labelmap -> avail option

val update_label : label -> avail -> labelmap -> labelmap * avail

val update_labels : label list -> avail -> labelmap -> labelmap

val is_debug_setvar : external_function -> ident option

val is_builtin_debug_setvar : instruction -> ident option

val transfer :
  labelmap -> avail option -> instruction -> labelmap * avail option

val ana_code : labelmap -> avail option -> code -> labelmap

val ana_iter : code -> labelmap -> (labelmap, labelmap) sum

val ana_function : coq_function -> labelmap option

val diff : avail -> avail -> avail

val delta_state : avail option -> avail option -> avail * avail

val add_start_range : (ident * debuginfo) -> code -> code

val add_end_range : (ident * debuginfo) -> code -> code

val add_delta_ranges : avail option -> avail option -> code -> code

val skip_debug_setvar : labelmap -> avail option -> code -> avail option

val transf_code : labelmap -> avail option -> code -> code

val transf_function : coq_function -> coq_function res

val transf_fundef : fundef -> fundef res

val transf_program : program -> program res
