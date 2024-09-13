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

(** [codegen sections v]: takes the abstract syntax tree representation of the
    task candidate vertex [v] and extends it with profiling instructions if the
    latter is an eligible task candidate, i.e. it carries the [Taskifiable]
    attribute.

    To profile a task candidate, we associate it with a profiling section. For
    example, let us apply [codegen] on the task candidate [v1] below.

    {[
    +-----------------------------+
    |            {v1}             |
    |       a = add(a, b);        |
    |       a = mul(a, b);        |
    |    (in: [b], inout: [a])    |
    +-----------------------------+
    ]}

    The pass takes its abstract syntax tree representation corresponding to the
    C source code,

    {[
    a = add(a + 1, b);
    a = mul(a, b);
    ]}

    generates the following additional statements involving the elements of
    the profiling header [!Apac_miscellaneous.profiler_hpp]

    - a declaration of a profiling section (see [apac_s] in the header),
    - a call to the initialization method of the profiling section (see
      [apac_s::initialize] in the header),
    - a series of calls to the [add] method of the profiling section (see
      [apac_s::add] in the header) recording the values of the task candidate
      parameters, i.e. [a + 1], [b] as well as [a] and [b], in this example,
    - a call to the [before] method (see [apac_s::before] in the header)
      initiating the measurement of the execution time of the task candidate,
    - a call to the [after] method (see [apac_s::after] in the header) stopping
      and recording the measurement of the execution time of the task candidate.

    and returns a list consisting of the above statements including the
    abstract syntax tree of [v1] in-between the call to [before] and the call to
    [after]. This list of statements corresponds to the following C source code.
      
    {[
    __apac_section30.initialize("30");
    __apac_section30.add(a + 1);
    __apac_section30.add(b);
    __apac_section30.add(a);
    __apac_section30.add(b);
    __apac_section30.before();  
    a = add(a + 1, b);
    a = mul(a, b);
    __apac_section30.after();
    ]}

    Here, [30] represents the hash of [v1] and allows us to uniquely identify
    the profiling section. During performance modelization (see [!modelize]),
    the parameters, e.g. [a + 1], [b] as well as [a] and [b], receive
    alternative generic variable names, i.e. [X0], [X1], [X2] and [X3],
    referring to the order in which we [add] them to the profiling section. We
    record the correspondence between the parameter terms, i.e. [a + 1], [b] as
    well as [a] and [b], and their modelization identifiers, i.e. [0], [1], [2]
    and [3], in the global hash table [!parameters]. Later, this allows us to
    replace the generic parameter names by the corresponding abstract syntax
    tree terms when transposing the results of performance modelization into the
    task candidate graph representation (see [!optimize]). Finally, the stack of
    terms [sections] keeps track of the profiling sections we generate. Indeed,
    we declare the profiling sections later at the top of the corresponding
    function's body rather than in-place. This is to prevent [goto] statements
    potenitally crossing declarations of profile sections leading to compilation
    errors. *)
let codegen (sections : trm Stack.t) (v : TaskGraph.V.t) : trms =
  (** [codegen.call c m args]: an auxiliary function to generate a call to a
      method [m] of the profiling section [c] while passing it [args] as
      arguments. *)
  let call (c : var) (m : label) (args : trms) : trm =
    let f = trm_struct_get (trm_get (trm_var c)) m in
    trm_apps f args
  in
  (** Retrieve the label [t] of the task candidate [v]. *)
  let t = TaskGraph.V.label v in
  (** If [v] does not represent an eligible task candidate, i.e. it does not
      carry the [Taskifiable] attribute, do not place [v] into a profiling
      section and return its abstract syntax tree representation as-is. *)
  if not (Task.attributed t Taskifiable) then
    t.current
  else
    (** Otherwise, start by computing the hash [h] of [v]. *)
    let h = TaskGraph.V.hash v in
    (** Keep also a string representation of [h] as [h']. *)
    let h' = string_of_int h in
    (** This allows us to generate a unique name as well as the corresponding
        variable for the new profiling section to surround [v] with. *)
    let s = Apac_macros.profiler_section ^ h' in
    let s' = new_var s in
    (** We then generate the declaration of the section variable without
        explicitly initializing it. *)
    let section = trm_let_mut (s', Typ.typ_str (Atyp "apac_s"))
                    (trm_uninitialized ()) in
    (** In the next step, we produce the [add] calls, in [params], for each
        parameter of the task candidate, we want to record the value of for the
        performance modelization. [i] increases with each call to [add] and
        represents the key in a [map] (see type [!type:Int_map.t]) keeping the
        track of the correspondence between a parameter abstract syntax tree
        term and its generic variable name within the performance modelization.
        We store this [map], for each task candidate of hash [h], in the global
        hash table [!parameters] for later processing in [!optimize]. *)
    let i = ref 0 in
    let (map, params) =
      (** We loop over each statement [c] within the task candidate [v] and *)
      List.fold_right (fun c (m, p) ->
          (** based on its kind, we decide which parameters we want to record
              for performance modelization. *)
          match c.desc with
          (** When [c] is a call to a function [f], *)
          | Trm_apps ({ desc = Trm_var (_ , f); _ }, args) ->
             (** we retrieve the function record as well as *)
             let r = Var_Hashtbl.find Apac_records.functions f in
             (** the list of arguments of the call (ignoring the reference to
                 [this] in the case of class memeber methods) and *)
             let args = if f.name = "this" then List.tl args else args in
             (** record the value, in the form of an abstract syntax tree term,
                 of each argument [arg] *)
             List.fold_right2 (fun arg (_, nli) (m, p) ->
                 (** which is a simple variable or a reference to the latter,
                     i.e. the number of levels of indirections of which is
                     smaller than [1] according to the corresponding argument
                     classification in [r.args]. *)
                 if nli < 1 then
                   begin
                     (** To this end, we, at first, create a new binding in the
                         map [m] (resulting in [map] at the end of the process)
                         between the ordinal of [arg], i.e. [i], and [arg]
                         itself. *)
                     let m = Int_map.add !i arg m in
                     (** We then generate a call to [apac_s::add] with [arg] as
                         an argument so as to effectively record the value [arg]
                         evaluates to at runtime for performance
                         modelization. *)
                     let p' = (call s' "add" [arg]) :: p in
                     incr i; (m, p')
                   end
                 else
                   (m, p)
               ) args r.args (m, p)
          (** TODO: Implement support for loops. *)
          | _ -> (m, p)
        ) t.current (Int_map.empty, []) in
    (** Finally, we produce the call to [apac_s::initialize] for the new
        profiling section as well as *)
    let opening = call s' "initialize" [trm_string h'] in
    (** the call to [apac_s::before] and *)
    let before = code (Instr (s ^ ".before()")) in
    (** the call to [apac_s::after]. *)
    let after = code (Instr (s ^ ".after()")) in
    (** We store the section declaration term in the [sections] stack and *)
    Stack.push section sections;
    (** the correspondences between parameter terms and their generic names in
        the hash table of maps [parameters] prior to *)
    Hashtbl.add parameters h map;
    (** returning the abstract syntax tree representation [t.current] of [v]
        surrounded with the profiling statements we have just built. *)
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
