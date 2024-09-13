open Ast
open Trm
open Target
open Tools
open Apac_dep
open Apac_tasks

(** [parameters]: a hash table of mappings between performance model parameters
    of type [!type:int] and their expressions in the source code of type
    [!type:trm] (see [!module:Int_map]). The table associates each mapping to a
    task candidate vertex through its hash (see type [!type:TaskGraph.V.t]). *)
let parameters = Hashtbl.create 97

(** [formulas]: a hash table of formulas for estimating execution times of task
    candidates. The table associates each formula to a task candidate vertex
    through its hash (see type [!type:TaskGraph.V.t]). *)
let formulas = Hashtbl.create 97

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
  let exception IllFormedModel of string in
  let exception UnknownParameter of int in
  let exception UnknownTaskCandidate of int in
  let exception IllFormedPower of string in
  let one (line : string) : int * string =
    let regex =
      Str.regexp "\\([0-9]+\\)(nbparams=[0-9]+)=\\(None\\|[^=\n]+\\)" in
    if Str.string_match regex line 1 then
      let vertex = Str.matched_group 1 line in
      let formula = Str.matched_group 2 line in
      (int_of_string vertex, formula)
    else
      raise (IllFormedModel line)
  in
  let process (model : string) : unit =
    let to_double (c : string) : trm =
      try
        let v = Val_lit (Lit_double (Float.of_string c)) in trm_val v
      with
      | Failure _ ->
         failwith ("[modelize]: `" ^ c ^ "' is not a valid floating-point \
                                          constant.")
    in
    let pow (p : trm Int_map.t) (op : string) : trm =
      let re = Str.regexp "apac_fpow<\\([0-9]+\\)>(X\\([0-9]+\\))" in
      if Str.string_match re op 0 then
        let exp = int_of_string (Str.matched_group 1 op) in
        let base = int_of_string (Str.matched_group 2 op) in
        if Int_map.mem base p then
          let exp = trm_val (Val_lit (Lit_int exp)) in
          let base = Int_map.find base p in
          let f = trm_toplevel_free_var "apac_fpow" in
          trm_apps f [exp; base]
        else
          raise (UnknownParameter base)
      else
        raise (IllFormedPower op)
    in
    let mul (p : trm Int_map.t) (op : string) : trm =
      let re = Str.regexp "\\*" in
      let ops = Str.split re op in
      let ops = List.map String.trim ops in
      let ops = List.map (fun op ->
                    if (String.starts_with ~prefix:"apac_fpow" op) then
                      pow p op
                    else if (String.starts_with ~prefix:"X" op) then
                      let id = int_of_string (String.make 1 op.[1]) in
                      if (Int_map.mem id p) then
                        Int_map.find id p
                      else
                        raise (UnknownParameter id)
                    else
                      to_double op
                  ) ops in
      List.fold_left (fun acc op -> trm_mul acc op) (List.hd ops) (List.tl ops)
    in
    let model' = open_in model in
    begin
      try
        ignore (input_line model')
      with
      | End_of_file ->
         begin
           close_in model';
           failwith ("[modelize]: `" ^ model ^ "' is not a valid model file.")
         end
    end;
    try
      while true do
        let line = input_line model' in
        let (v, f) = one line in
        if (Hashtbl.mem parameters v) then
          let p = Hashtbl.find parameters v in
          if f <> "None" then
            let re = Str.regexp "\\+" in
            let ops = Str.split re f in
            let ops = List.map String.trim ops in
            let hd = List.hd ops in
            let hd = to_double hd in
            let ops = List.tl ops in
            let ft = List.fold_left (fun acc op ->
                         let op = if (String.contains op '*') then mul p op
                                  else to_double op in
                         trm_add acc op
                       ) hd ops in
            Hashtbl.add formulas v (Some ft)
          else
            Hashtbl.add formulas v None
        else
          raise (UnknownTaskCandidate v)
      done
    with
    | End_of_file -> close_in model'
    | IllFormedModel m ->
       failwith ("[modelize]: `" ^ m ^ "' is not a valid performance model.")
    | UnknownParameter p ->
       failwith ("[modelize]: unknown parameter `" ^ (string_of_int p) ^ "'.")
    | UnknownTaskCandidate c ->       
       failwith
         ("[modelize]: unknown task candidate `" ^ (string_of_int c) ^ "'.")
    | IllFormedPower p ->
       failwith ("[modelize]: `" ^ p ^ "' is not a valid power expression.")
    | e -> failwith
             ("[modelize]: unexpected error occurred (" ^
                (Printexc.to_string e) ^ ")")
  in
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
          else
            let _ = process model in
            Hashtbl.iter (fun v f ->
                match f with
                | Some f -> Printf.printf "%d: %s\n" v (AstC_to_c.ast_to_string f)
                | None -> Printf.printf "%d: None\n" v
              ) formulas
          
    ) tg

let optimize (tg : target) : unit =
  let rec aux (v : TaskGraph.V.t) : unit =
    let t = TaskGraph.V.label v in
    if (Task.attributed t Taskifiable) then
      begin
        let h = TaskGraph.V.hash v in
        let f =
          try
            Hashtbl.find formulas h
          with
          | Not_found ->
             failwith ("Apac_profiler.optimize: task candidate `" ^
                         (Task.to_string t) ^ "' has no performance model.") in
        if Option.is_none f then
          t.attrs <- TaskAttr_set.remove Taskifiable t.attrs
        else
          let const = trm_fold (fun acc t ->
                          match t.desc with
                          | Trm_apps _ -> acc && false
                          | _ -> acc && true
                        ) true (Option.get f) in
          if (not const) then
            t.cost <- f
      end;
    (** When [v] features nested candidate graphs, explore the substatements. *)
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter aux go
          ) gl
      ) t.children      
  in
  Target.iter (fun t p ->
      (** Find the parent function [f]. *)
      let f = match (find_parent_function p) with
        | Some (v) -> v
        | None -> fail t.loc "Apac_profiler.optimize: unable to find parent \
                              function. Task group outside of a function?" in
      (** Find its function record [r] in [!Apac_records.functions]. *)
      let r = Var_Hashtbl.find Apac_records.functions f in
      (** Optimize the task candidate graph [r.graph] of [f] according to its
          performance model, i.e. the execution time estimation [formulas]. *)
      TaskGraphTraverse.iter aux r.graph;
      (** Dump the resulting task candidate graph, if requested. *)
      if !Apac_macros.verbose then
        begin
          Printf.printf "Task candidate graph of `%s' (optimization):\n"
            (var_to_string f);
          TaskGraphPrinter.print r.graph
        end;
      if !Apac_macros.keep_graphs then
        TaskGraphExport.to_pdf
          r.graph (Apac_miscellaneous.gf ~suffix:"optimization" f)
    ) tg
