include Optitrust_utils
include Optitrust_ast
include Ast
include Trm
include Typ
include Contextualized_error
include Mark
(* include Target *)
include Trm_pattern

module Trm = struct
  include Trm
  (* short aliases *)
  let var = trm_var
  let struct_access = trm_struct_access
  let array_access = trm_array_access
  let get_stringreprid = trm_get_stringreprid

  let pattern_var = trm_pattern_var
end

(* module Show.At = Show.At *)

let debug_path = true

let trm_seq_nobrace = Nobrace.trm_seq
let trm_seq_nobrace_nomarks = Nobrace.trm_seq_nomarks

type seq_component =
  | Trm of trm
  | TrmList of trm list
  | TrmMlist of trm mlist
  | Mark of mark
  | MarkList of mark list
  | SeqComponents of seq_component list

let trm_seq_helper ?(annot : trm_annot option) ?(loc : location) ?(result: var option) ?(braces = true) (components: seq_component list) : trm =
  let rec aux cs acc = List.fold_right (fun comp acc ->
    match comp with
    | Trm t -> Mlist.push_front t acc
    | TrmList tl -> Mlist.merge (Mlist.of_list tl) acc
    | TrmMlist tml -> Mlist.merge tml acc
    | Mark "" -> acc
    | Mark m -> Mlist.insert_mark_at 0 m acc
    | MarkList ms -> Mlist.insert_marks_at 0 ms acc
    | SeqComponents cs -> aux cs acc
  ) cs acc in
  let mlist = aux components (Mlist.empty ()) in
  if braces then
    trm_seq ?annot ?loc ?result mlist
  else begin
    assert (annot = None);
    assert (loc = None);
    trm_seq_nobrace ?result mlist
  end

let update_span_helper (span : Dir.span) (t_seq : trm) (f : trm mlist -> seq_component list) : trm =
  let instrs, result = trm_inv ~error:"expected seq" trm_seq_inv t_seq in
  if span.start >= span.stop then begin
    t_seq
  end else begin
    let (span_instrs, instrs_after) = Mlist.split ~left_bias:false span.stop instrs in
    let (instrs_before, span_instrs) = Mlist.split ~left_bias:true span.start span_instrs in
    let new_span_components = f span_instrs in
    trm_seq_helper ~annot:t_seq.annot ?result [
      TrmMlist instrs_before;
      SeqComponents new_span_components;
      TrmMlist instrs_after;
    ]
  end

let skip_includes (t : trm) : trm =
  match trm_seq_inv t with
  | Some (instrs, None) ->
    let not_include = Mlist.filter (fun t -> not (trm_is_include t)) instrs in
    trm_seq not_include
  | _ -> failwith "skip_includes should be called on the root of the AST"

