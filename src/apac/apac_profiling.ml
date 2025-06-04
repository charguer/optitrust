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

(** [annotate tg]: expects the target [tg] to point at a function body. It then
    translates its task candidate graph representation into an abstract syntax
    tree while annotating eligible task candidates, i.e. those carrying the
    [Taskifiable] attribute, with profiling sections.

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
    the profiling header [!Apac_macros.profile_hpp]

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
let annotate (tg : target) : unit =
  (** Include the header providing profiling elements. *)
  Trace.ensure_header Apac_macros.profile_include;
  (** Perform the translation. *)
  Target.apply (fun t p ->
      Path.apply_on_path (fun t ->
          (** Find the parent function [f]. *)
          let f = match (Apac_miscellaneous.find_parent_function p) with
            | Some (v) -> v
            | None -> fail t.loc "Apac_profiling.annotate: unable to find \
                                  parent function. Taskification candidate \
                                  body outside of a task candidate?" in
          (** Find its function record [r] in [!Apac_records.functions]. *)
          let r = Var_Hashtbl.find Apac_records.functions f in
          (** Retrieve its scope. *)
          let scope = r.scope in
          (** Initialize a stack [sections] for storing the definitions of
              future profiling sections. *)
          let sections = Stack.create () in
          (** Translate the task candidate graph representation [r.graph] of [f]
              to an abstract syntax tree. *)
          let ast =
            TaskGraphTraverse.to_ast
              (fun v ->
                (** [annotate.<anonymous>.call c m args]: an auxiliary function
                    to generate a call to a method [m] of the profiling section
                    [c] while passing it [args] as arguments. *)
                let call (c : var) (m : label) (args : trms) : trm =
                  let f = trm_struct_get (trm_get (trm_var c)) m in
                  trm_apps f args
                in
                (** Retrieve the label [t] of the task candidate [v]. *)
                let t = TaskGraph.V.label v in
                (** If [v] does not represent an eligible task candidate, i.e.
                    it does not carry the [Taskifiable] attribute, do not place
                    [v] into a profiling section and return its abstract syntax
                    tree representation as-is. *)
                if not (Task.attributed t Taskifiable) then
                  t.current
                else
                  (** Otherwise, start by computing the hash [h] of [v]. *)
                  let h = TaskGraph.V.hash v in
                  (** Keep also a string representation of [h] as [h']. *)
                  let h' = string_of_int h in
                  (** This allows us to generate a unique name as well as the
                      corresponding variable for the new profiling section to
                      surround [v] with. *)
                  let s = Apac_macros.profile_section_prefix ^ h' in
                  let s' = new_var s in
                  (** We then generate the declaration of the section variable
                      without explicitly initializing it. *)
                  let section =
                    trm_let_mut (
                        s', Typ.typ_str (Atyp Apac_macros.profile_section_type)
                      ) (trm_uninitialized ()) in
                  (** In the next step, we produce the [add] calls, in [params],
                      for each parameter of the task candidate we want to record
                      the value of for the performance modelling. [i] increases
                      with each call to [add] and represents the key in a [map]
                      (see type [!type:Int_map.t]) keeping the track of the
                      correspondence between a parameter abstract syntax tree
                      term and its generic variable name within the performance
                      modelization. We store this [map], for each task candidate
                      of hash [h], in the global hash table [!parameters] for
                      later processing in [!optimize]. Note that we use the
                      [prev] set to store the C expression of each argument we
                      process so as to prevent duplicates in [m]. This can
                      happen, for example, in the following case [foo(a, a, b);]
                      or in within a task candidate consisting of two function
                      calls [foo(a,b); bar(a, c);]. Duplicates in [m] complexify
                      the resulting execution time formulas as the modelizer
                      considers them as separate parameters. *)
                  let i = ref 0 in
                  let prev = ref String_set.empty in
                  let (map, params) =
                    (** We loop over each statement [t'] within the task
                        candidate [v] and based on its kind and the kind of its
                        substatements, if any, we decide which parameters we
                        want to record for performance modelization. *)
                    List.fold_right (fun t' (m, p) ->
                        trm_fold (fun (m, p) c ->
                            match c.desc with
                            (** When [c] is a call to a function [f], *)
                            | Trm_apps (
                                { desc = Trm_var (_ , f); _ }, args
                              ) when Var_Hashtbl.mem Apac_records.functions f ->
                               (** we retrieve the function record as well as *)
                               let r =
                                 Var_Hashtbl.find Apac_records.functions f in
                               (** the list of arguments of the call (ignoring
                                   the reference to [this] in the case of class
                                   memeber methods) and *)
                               let args =
                                 if f.name = "this" then
                                   List.tl args
                                 else args
                               in
                               (** record the value, in the form of an abstract
                                   syntax tree term, of each [profilable] (see
                                   [!Apac_records.FunctionRecord.create])
                                   argument [arg] according to the corresponding
                                   argument classification in [r.args] which
                                   features at least one variable occurrence
                                   (see the flag [hv]), i.e. which is not
                                   or does not lead to a run-time constant. *)
                               List.fold_right2 (fun arg (_, _, profilable)
                                                     (m, p) ->
                                   let hv =
                                     trm_fold (fun acc st ->
                                         match st.desc with
                                         | Trm_var _ -> acc || true
                                         | _ -> acc
                                       ) false arg
                                   in
                                   if profilable && hv then
                                     begin
                                       (** To this end, we, at first, create a
                                           new binding in the map [m] (resulting
                                           in [map] at the end of the process)
                                           between the ordinal of [arg], i.e.
                                           [i], and [arg] itself. At the same
                                           time, we convert [arg] back to C
                                           syntax in [arg'] as
                                           [!Ast_fromto_AstC.caddress_intro] and
                                           [!Ast_fromto_AstC.stackvar_intro] do
                                           not apply on terms within OpenMP
                                           directives. *)
                                       let arg' =
                                         Apac_records.restore_cfeatures
                                           scope arg
                                       in
                                       (** [arg''] is the string representation
                                           of [arg'] to interact with [prev]. *)
                                       let arg'' =
                                         AstC_to_c.ast_to_string arg'
                                       in
                                       (** However, we create a new binding only
                                           if the C expression [arg'] appears in
                                           the list of arguments [args] for the
                                           first time. *)
                                       if not (String_set.mem arg'' !prev) then
                                         begin
                                           let m = Int_map.add !i arg' m in
                                           (** We then generate a call to
                                               [apac_s::add] with [arg] as an
                                               argument so as to effectively
                                               record the value [arg] evaluates
                                               to at runtime for performance
                                               modelization. *)
                                           let p' =
                                             (call
                                                s'
                                                Apac_macros.profile_section_add
                                                [arg]
                                             ) :: p in
                                           incr i;
                                           prev := String_set.add arg'' !prev;
                                           (m, p')
                                         end
                                       else
                                         (m, p)
                                     end
                                   else
                                     (m, p)
                                 ) args r.args (m, p)
                            (** TODO: Implement support for loops. *)
                            | _ -> (m, p)
                          ) (m, p) t'
                      ) t.current (Int_map.empty, []) in
                  (** Finally, we produce the call to [apac_s::initialize] for
                      the new profiling section as well as *)
                  let opening =
                    call s' Apac_macros.profile_section_init [trm_string h'] in
                  (** the call to [apac_s::before] and *)
                  let before = code
                                 (Instr
                                    (s ^ "." ^
                                       Apac_macros.profile_section_before ^
                                         "()")) in
                  (** the call to [apac_s::after]. *)
                  let after = code
                                (Instr
                                   (s ^ "." ^
                                      Apac_macros.profile_section_after ^
                                        "()")) in
                  (** We store the section declaration term in the [sections]
                      stack and *)
                  Stack.push section sections;
                  (** the correspondences between parameter terms and their
                      generic names in the hash table of maps [parameters] prior
                      to *)
                  Hashtbl.add parameters h map;
                  (** returning the abstract syntax tree representation
                      [t.current] of [v] surrounded with the profiling
                      statements we have just built. *)
                  opening :: (params @ [before] @ t.current @ [after])
              ) r.graph in
          let sections = List.of_seq (Stack.to_seq sections) in
          let ast = Mlist.of_list (sections @ ast) in
          let result = trm_seq ~annot:t.annot ~ctx:t.ctx ast in
          (** Dump the resulting abstract syntax tree, if requested. *)
          if !Apac_flags.verbose then
            begin
              let msg = Printf.sprintf
                          "Abstract syntax tree of `%s' with profiling \
                           instructions" (var_to_string f) in
              Debug_transfo.trm msg result
            end;
          (** Return the resulting abstract syntax tree. *)
          result
        ) t p) tg

(** [annotate_main tg]: expects the target [tg] to point at the body of the
    [!Apac_flags.main] function. It then places the body into the meta profiling
    section [!Apac_macros.profile_section_main]. To this end, the pass surrounds
    the body with the following additional statements involving the elements of
    the profiling header [!Apac_macros.profile_hpp]:

    - a declaration of the [!Apac_macros.profile_section_main] profiling section
      (see [apac_s] in the header),
    - a call to the initialization method of the profiling section (see
      [apac_s::initialize] in the header),
    - a call to the [before] method (see [apac_s::before] in the header)
      initiating the measurement of the execution time of the underlying
      statements,
    - a call to the [after] method (see [apac_s::after] in the header) stopping
      and recording the measurement of the execution time of the underlying
      statements.

    For example:

    {[
    int main() {
      int * t = init_data(); 
      c(t, 4);
      free(t);
      return 0;
    }
    ]}

    translates to:
      
    {[
    int main() {
      __apac_section_main.initialize("__apac_section_main");
      __apac_section_main.before();
      int * t = init_data(); 
      c(t, 4);
      free(t);
      __apac_section_main.after();
      return 0;
    ]} *)
let annotate_main (tg : target) : unit =
  (** [annotate_main.call c m args]: an auxiliary function to generate a call to
      a method [m] of the profiling section [c] while passing it [args] as
      arguments. *)
  let call (c : var) (m : label) (args : trms) : trm =
    let f = trm_struct_get (trm_get (trm_var c)) m in
    trm_apps f args
  in
  Target.apply_at_target_paths (fun t ->
      (** Create a variable for the [!Apac_macros.profile_section_main]
          section. *)
      let m = new_var Apac_macros.profile_section_main in
      (** Declare the section variable. *)
      let declaration =
        trm_let_mut (
            m, Typ.typ_str (Atyp Apac_macros.profile_section_type)
          ) (trm_uninitialized ())
      in
      (** Produce the call to [apac_s::initialize] for the meta profiling section
          as well as *)
      let opening =
        call m Apac_macros.profile_section_init
          [trm_string Apac_macros.profile_section_main]
      in
      (** the call to [apac_s::before] and *)
      let before =
        code
          (Instr
             (Apac_macros.profile_section_main ^ "." ^
                Apac_macros.profile_section_before ^
                  "()"))
      in
      (** the call to [apac_s::after]. *)
      let after =
        code
          (Instr
             (Apac_macros.profile_section_main ^ "." ^
                Apac_macros.profile_section_after ^
                  "()"))
      in
      (** Deconstruct the body term. *)
      let error =
        "Apac_profiling.annotate_main: expected a target to a function body."
      in
      let b = trm_inv ~error trm_seq_inv t in
      (** Surround the original sequence of the body with the meta profiling
          section terms. *)
      let b = Mlist.insert_sublist_at 0 [declaration; opening; before] b in
      (** If the [l]ast statement of the sequence is a `return' statement, place
          the call to [apac_s::after] before the latter. *)
      let i = (Mlist.length b) - 1 in
      let l = Mlist.nth b i in
      let b =
        if is_return l then
          Mlist.insert_at i after b
        else
          Mlist.push_back after b
      in
      (** Rebuild the body term and return it. *)
      trm_seq ~annot:t.annot ~ctx:t.ctx b
    ) tg

(** [modelize tg]: expects the target [tg] to point at the entire abstract
    syntax tree featuring profiling instructions. It then generates the
    corresponding C source code, compiles it, runs it and modelizes the
    execution time of the different profiling sections in it. *)
let modelize (tg : target) : unit =
  (** [IllFormedModel, UnknownParameter, UnknownTaskCandidate, IllFormedPower]:
      custom exceptions for handling local-specific errors. *)
  let exception IllFormedModel of string in
  let exception UnknownParameter of int in
  let exception UnknownTaskCandidate of int in
  let exception IllFormedPower of string in
  (** [modelize.one line]: an auxiliary function to parse one [line] of an
      execution time model and return the profiling section identifier, i.e. the
      hash of the corresponding task candidate vertex, as well as the formula,
      in its string form, for estimating the execution time of the latter. If
      the parsing fails, it raises [IllFormedModel] with the problematic [line]
      as an argument. *)
  let one (line : string) : int * string =
    let regex = Str.regexp Apac_macros.model_re in
    if Str.string_match regex line 0 then
      let vertex = Str.matched_group 1 line in
      let formula = Str.matched_group 2 line in
      (int_of_string vertex, formula)
    else
      raise (IllFormedModel line)
  in
  (** [modelize.process model]: an auxiliary function to read an execution time
      [model] file line-by-line, extract and transpile the execution time
      estimation formulas into their abstract syntax tree form and store them in
      the global hash table [formulas]. *)
  let process (model : string) : unit =
    (** [modelize.process.to_double c]: an auxiliary function to transpile the
        string representation of the double floating-point value [c] into an
        abstract syntax tree literal value term. *)
    let to_double (c : string) : trm =
      try
        let v = Val_lit (Lit_double (Float.of_string c)) in trm_val v
      with
      | Failure _ ->
         failwith ("Apac_profiling.modelize: `" ^ c ^
                     "' is not a valid floating-point constant.")
    in
    (** [modelize.process.pow p op]: an auxiliary function to transpile the
        string representation [op] of a power expression consisting of a call to
        the [apac_fpow] function (see [!Apac_epilogue.dynamic_cutoff]) into an
        adequate abstract syntax tree term. The function also replaces any
        occurence to the generic variable name of a profiling parameter matching
        [X\[0-9\]+] in [op] by the corresponding abstract syntax tree term
        thanks to the binding map [p] (see [!parameters] and [!codegen]). If
        there is no such binding in [p], the function raises [UnknownParameter]
        and passes the problematic identifier, i.e. the integer following [X],
        as an argument. If it fails to decode the power expression, it raises
        [IllFormedPower] and passes [op] as an argument. *)
    let pow (p : trm Int_map.t) (op : string) : trm =
      let re = Str.regexp Apac_macros.model_pow_re in
      if Str.string_match re op 0 then
        let exp = int_of_string (Str.matched_group 1 op) in
        let base = int_of_string (Str.matched_group 2 op) in
        if Int_map.mem base p then
          begin
            (** If [op] contains a valid power expression, it means we'll need
                to include the definition [!Apac_macros.pow] in the resulting
                source code, so we have to tell the compiler to do so through
                the [!Apac_records.put_pow] flag. *)
            Apac_records.put_pow := true;
            let exp = trm_val (Val_lit (Lit_int exp)) in
            let base = Int_map.find base p in
            (** Building the power function variable term using
                [trm_toplevel_free_var] allows us to use it without declaring it
                in the abstract syntax tree of the input program. *)
            let f = trm_toplevel_free_var Apac_macros.model_pow in
            (** The resulting call is like [apac_fpow(exponent, base)]. *)
            trm_apps f [exp; base]
          end
        else
          raise (UnknownParameter base)
      else
        raise (IllFormedPower op)
    in
    (** [modelize.process.mul p op]: an auxiliary function to transpile the
        string representation [op] of a multiplication expression into an
        adequate abstract syntax tree term. The function also replaces any
        occurence to the generic variable name of a profiling parameter matching
        [X\[0-9\]+] in [op] by the corresponding abstract syntax tree term
        thanks to the binding map [p] (see [!parameters] and [!codegen]). If
        there is no such binding in [p], the function raises [UnknownParameter]
        and passes the problematic identifier, i.e. the integer following [X],
        as an argument. *)
    let mul (p : trm Int_map.t) (op : string) : trm =
      let re = Str.regexp "\\*" in
      let ops = Str.split re op in
      let ops = List.map String.trim ops in
      let ops = List.map (fun op ->
                    if (String.starts_with
                          ~prefix:Apac_macros.model_pow op) then
                      pow p op
                    else if (String.starts_with
                               ~prefix:Apac_macros.model_parameter op) then
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
    (** This is the core of the [process] function. We simply read [model]
        line-by-line until the end of the file. *)
    let model' = open_in model in
    try
      while true do
        (** For each [line] we read, *)
        let line = input_line model' in
        (** we extract the profiling section identifier, i.e. the hash [v] of
            the corresponding task candidate vertex, as well as the formula [f],
            in its string form, for estimating the execution time of the
            latter. *)
        let (v, f) = one line in
        (** If [!parameters] has a binding for [v], retrieve it. *)
        if (Hashtbl.mem parameters v) then
          let p = Hashtbl.find parameters v in
          (** If the modelization judges [v] is worth of becoming a
              parallelizable task, *)
          if f <> Apac_macros.model_na then
            (** transpile the formula from its string form into the
                corresponding abstract syntax tree term and *)
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
            (** store it for [v] in [formulas]. *)
            Hashtbl.add formulas v (Some ft)
          else
            (** Otherwise, indicate in [formulas] that [v] has no execution time
                estimation formula associated to it. *)   
            Hashtbl.add formulas v None
        else
          (** Otherwise, raise [UnknownTaskCandidate] and pass the problematic
              hash [v] as an argument. *)
          raise (UnknownTaskCandidate v)
      done
    with
    (** Close [model] when we reach the end of the file. *)
    | End_of_file -> close_in model'
    (** Other exceptions indicate an irreparable error. Show an adequate error
        message. *)
    | IllFormedModel m ->
       failwith ("Apac_profiling.modelize: `" ^ m ^
                   "' is not a valid performance model.")
    | UnknownParameter p ->
       failwith ("Apac_profiling.modelize: unknown parameter `" ^
                   (string_of_int p) ^ "'.")
    | UnknownTaskCandidate c ->       
       failwith
         ("Apac_profiling.modelize: unknown task candidate `" ^
            (string_of_int c) ^ "'.")
    | IllFormedPower p ->
       failwith ("Apac_profiling.modelize: `" ^ p ^
                   "' is not a valid power expression.")
    | e -> failwith
             ("Apac_profiling.modelize: unexpected error occurred (" ^
                (Printexc.to_string e) ^ ")")
  in
  (** This is the core of the [modelize] pass. *)
  Target.iter_at_target_paths (fun t ->
      (** Generate file names for the [header] with profiling class definitions,
          the temporary source [code] file representing the program with
          profiling instructions, the corresponding [binary], [profile] and
          execution time [model] files. *)
      let (header, code, binary, profile, model) =
        Apac_macros.runtime_analysis_files () in
      (** Generate file names for logs from building [code], running [binary]
          and computing an execution time model from [profile]. *)
      let (build, run, modeling) = Apac_macros.runtime_analysis_logs () in
      (** Synthesize the contents of the header with profiling class definitions
          and save it as [header]. *)
      Apac_macros.profile_hpp profile header;
      (** Take the current abstract syntax tree of the program and output it as
          C++ source code to [code]. *)
      let context = Trace.get_context () in
      Trace.output_prog ~ast_and_enc:false context code t;
      (** Remove the header with profiling class definitions from the current
          context, so it does not appear in the final parallel source code. We
          have just produced the source code using the header, so we do not need
          it in the current context anymore. *)
      Trace.drop_header Apac_macros.profile_include;
      (** Compile [code] into [binary]. *)
      let err = Sys.command (
                    Apac_macros.compile_cmdline
                      (code ^ context.extension) binary ^ " > " ^ build
                  ) in
      if err <> 0 then
        failwith "Apac_profiling.modelize: could not compile source code with \
                  profiling instructions."
      else
        (** Run [binary] which produces [profile]. *)
        let err = Sys.command (
                      Apac_macros.profile_cmdline binary ^ " > " ^ run
                    ) in
        if err <> 0 then
          failwith "Apac_profiling.modelize: could not perform the profiling \
                    run."
        else
          (** Run the modelizer script on [profile] which produces [model]. *)
          let err = Sys.command (
                        Apac_macros.model_cmdline profile model ^ " > " ^
                          modeling
                      ) in
          if err <> 0 then
            failwith "Apac_profiling.modelize: could not compute execution \
                      time model."
          else
            begin
              (** Parse [model]. *)
              process model;
              (** Dump the resulting execution time estimation formulas when in
                  verbose mode. *)
              if !Apac_flags.verbose then
                begin
                  Printf.printf
                    "Execution time estimation formulas (task candidate hash, \
                     formula):\n";
                  Hashtbl.iter (fun v f ->
                      match f with
                      | Some f ->
                         Printf.printf "(%d: %s)\n" v
                           (AstC_to_c.ast_to_string f)
                      | None ->
                         Printf.printf "(%d: %s)\n" v
                           Apac_macros.model_na
                    ) formulas
                end
            end          
    ) tg

(** [optimize tg]: expects the target [tg] to point at a function body. It then
    optimizes the task candidate graph of the function according to the results
    of execution time modelization. *)
let optimize (tg : target) : unit =
  (** [optimize.aux v]: an auxiliary function to recursively optimize a task
      candidate vertex [v]. *)
  let rec aux (v : TaskGraph.V.t) : unit =
    (** Get the label [t] of [v]. *)
    let t = TaskGraph.V.label v in
    (** Proceed only if [v] is an eligible task candidate, i.e. it carries the
        [Taskifiable] attribute. *)
    if (Task.attributed t Taskifiable) then
      begin
        (** In such case, compute the hash of [v] *)
        let h = TaskGraph.V.hash v in
        let f =
          (** so as to retrieve the corresponding execution time estimation
              formula in [!formulas]. *)
          try
            Hashtbl.find formulas h
          with
          (** If there is no corresponding formula in [!formulas], it means that
              the modelizer has filtered the task candidate out due to its
              supposedly negligible impact on the overall execution time of the
              program. *)
          | Not_found -> None
        in
        (** If there is no formula for [v], the modelization judges [v] is not
            an eligible candidate. *)
        if Option.is_none f then
          (** In this case, remove the [Taskifiable] attribute from its set of
              task candidate attributes. *)
          t.attrs <- TaskAttr_set.remove Taskifiable t.attrs
        else
          (** Otherwise, check whether the formula consists of a constant
              expression, i.e. does not feature any function calls or other
              operations. *)
          let const = trm_fold (fun acc t ->
                          match t.desc with
                          | Trm_apps _ -> acc && false
                          | _ -> acc && true
                        ) true (Option.get f) in
          (** Attribute [f] to [v] only if [f] does not consist of a constant
              expression. In the opposite case, the modelization judges [v] an
              eligible task candidate with a constant execution time. Therefore,
              we do not need to control whether [v] becomes a parallelizable
              task at runtime. We already know it's worth it. *)
          if (not const) then
            begin
              (** Also, tell the compiler that we have at least one non-constant
                  formula in the task candidate graph. *)
              Apac_records.put_cutoff := true;
              t.cost <- f
            end
      end;
    (** When [v] features nested candidate graphs, process the substatements. *)
    List.iter (fun gl ->
        List.iter (fun go ->
            TaskGraphTraverse.iter aux go
          ) gl
      ) t.children      
  in
  Target.iter (fun t p ->
      (** Find the parent function [f]. *)
      let f = match (Apac_miscellaneous.find_parent_function p) with
        | Some (v) -> v
        | None -> fail t.loc "Apac_profiling.optimize: unable to find parent \
                              function. Task group outside of a function?" in
      (** Find its function record [r] in [!Apac_records.functions]. *)
      let r = Var_Hashtbl.find Apac_records.functions f in
      (** Optimize the task candidate graph [r.graph] of [f] according to its
          performance model, i.e. the execution time estimation [formulas]. *)
      TaskGraphTraverse.iter aux r.graph;
      (** Dump the resulting task candidate graph, if requested. *)
      if !Apac_flags.verbose then
        begin
          Printf.printf "Task candidate graph of `%s' (optimization):\n"
            (var_to_string f);
          TaskGraphPrinter.print r.graph
        end;
      if !Apac_flags.keep_graphs then
        TaskGraphExport.to_pdf
          r.graph (Apac_macros.gf ~suffix:"optimization" f)
    ) tg
