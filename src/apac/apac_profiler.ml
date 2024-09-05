open Ast
open Trm
open Target
open Apac_dep
open Apac_tasks

(** [next_id]: generates unique integer identifiers starting from zero. *)
let next_id = Tools.fresh_generator ()

(** [next_profsection]: generates unique names for profiling sections used by
    the profiler backend. See [emit_profiler_task]. *)
let next_profsection () : string =
  let id = next_id () in
  "apac_profsection" ^ (string_of_int id)

let codegen_profiler (v : TaskGraph.V.t) : trms =
  let t = TaskGraph.V.label v in
  let get_begin (loc : location) : string =
    match loc with
    | None -> "_"
    | Some { loc_start = {pos_line = line; _}; _} -> string_of_int line
  in
  let get_end (loc : location) : string =
    match loc with
    | None -> "_"
    | Some { loc_end = {pos_line = line; _}; _} -> string_of_int line
  in
  if not (Task.attributed t Taskifiable) then t.current
  else
    let simple = fun d ->
      match d with
      | Dep_var v when (Var_Hashtbl.find scope v) = 0 -> true
      | _ -> false
    in
    let ins = Dep_set.filter simple t.ins in
    let inouts = Dep_set.filter simple t.inouts in
    let reads = Dep_set.cardinal ins in
    let writes = Dep_set.cardinal inouts in
    let first = List.hd t.current in
    let first = get_begin first.loc in
    let last = (List.length t.current) - 1 in
    let last = List.nth t.current last in
    let last = get_end last.loc in
    let profsection = next_profsection () in
    let range = if first <> "_" && last <> "_" then
                  first ^ "_" ^ last
                else profsection in
    let section = "ApacProfilerSection " ^ profsection ^ "(\"" ^
                   range ^ "\", " ^ (string_of_int (reads + writes)) ^ ")" in
    let section = code (Instr section) in
    let encode = fun m d ->
      let d = Dep.to_string d in
      let s = profsection ^ ".addParam(\'" ^ m ^ "\', " ^ d ^ ")" in
      code (Instr s)
    in
    let ins = Dep_set.to_list ins in
    let ins = List.map (encode "R") ins in
    let inouts = Dep_set.to_list inouts in
    let inouts = List.map (encode "R") inouts in (** W mode is not recognized in the modelizer. *)
    let before = code (Instr (profsection ^ ".beforeCall()")) in
    let after = code (Instr (profsection ^ ".afterCall()")) in
    Stack.push section sections;
    let preamble = ins @ inouts @ [before] in
    let postamble = [after] in
    preamble @ t.current @ postamble
