open Ast
open Trm
open Target
open Tools
open Apac_dep
open Apac_tasks

(** [parameters]: a hash table of mappings between performance model parameters
    of type [!type:int] and their expressions in the source code of type
    [!type:trm] (see [!module:Int_map]). The table associates each mapping to a
    task candidate. To achieve this, the table uses task candidate hashes of
    type [!type:int] (see [!TaskGraph.V.hash]) as keys. *)
let parameters = Hashtbl.create 10

let codegen_profiler (sections : trm Stack.t) (v : TaskGraph.V.t) : trms =
  let call (c : var) (m : label) (args : trms) : trm =
    let f = trm_struct_get (trm_get (trm_var c)) m in
    trm_apps f args
  in
  let t = TaskGraph.V.label v in
  if not (Task.attributed t Taskifiable) then
    t.current
  else
    let h = TaskGraph.V.hash v in
    let h' = string_of_int h in
    let s = Apac_macros.profiler_section ^ h' in
    let s' = new_var s in
    let section = trm_let_mut (s', Typ.typ_str (Atyp "apac_s"))
                    (trm_uninitialized ()) in
    let i = ref 0 in
    let (map, params) =
      List.fold_right (fun c (m, p) ->
          match c.desc with
          | Trm_apps ({ desc = Trm_var (_ , f); _ }, args) ->
             let r = Var_Hashtbl.find Apac_records.functions f in
             let args = if f.name = "this" then List.tl args else args in
             List.fold_right2 (fun arg (_, nli) (m, p) ->
                 if nli < 1 then
                   begin
                     let m = Int_map.add !i arg m in
                     let p' = (call s' "add" [arg]) :: p in
                     incr i; (m, p')
                   end
                 else
                   (m, p)
               ) args r.args (m, p)
          | _ -> (m, p)
        ) t.current (Int_map.empty, []) in
    let opening = call s' "initialize" [trm_string h'] in
    let before = code (Instr (s ^ ".before()")) in
    let after = code (Instr (s ^ ".after()")) in
    Stack.push section sections;
    Hashtbl.add parameters h map;
    opening :: (params @ [before] @ t.current @ [after])

let modelize (tg : target) : unit =
  Target.iter_at_target_paths (fun t ->
      let (header, code, binary, profile, model) =
        Apac_miscellaneous.hcbpm () in
      Apac_miscellaneous.profiler_hpp profile header;
      Trace.output_prog ~ast_and_enc:false (Trace.get_context ()) code t;
      let err = Sys.command ("g++ -o " ^ binary ^ " " ^ code ^ ".cpp") in
      if err <> 0 then
        failwith "[modelize]: could not compile source code with profiling \
                  instructions."
      else
        let err = Sys.command binary in
        if err <> 0 then
          failwith "[modelize]: could not perform the profiling run."
        else
          let err = Sys.command ("python3 -m apac_modelizer " ^ profile) in
          if err <> 0 then
            failwith "[modelize]: could not compute execution time model."
          
    ) tg
