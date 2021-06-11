
(******************************************************************************)
(*                        Debug                                               *)
(******************************************************************************)

(* include Tools.Debug *)
include Tools (* TODO: make it better *)

let set_exn_backtrace (b : bool) : unit =
  Printexc.record_backtrace b

(* By default, we want backtrace for exceptions *)
let _ = set_exn_backtrace true



(******************************************************************************)
(*                              Run                                           *)
(******************************************************************************)

(* [script f] serves as "main" function for an Optitrust script. It cakes care
   of parsing the command line arguments and handling the errors, in addition
   to running the function [f] provided. *)
let script (f : unit -> unit) : unit =
  Arg.parse
    Flags.spec
    (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    ("usage: no argument expected, only options");
  try
    f ();
    Trace.close_logs()
  with | Failure s ->
    Trace.close_logs();
    failwith s

(* [script_cpp f] is a specialized version of [script f] that:
   - automatically invokes [Trace.init "foo.cpp"] at start,
     where "foo" is the basename of the current script named "foo.ml";
   - automatically invokes [Trace.dump] at the end of the script;
     (the main output file is named "foo_out.cpp"). *)
let script_cpp ?(prefix : string = "") (f : unit -> unit) : unit =
  (* Extract the basename. We remove "_with_lines" suffix if the basename ends with that suffix. *)
  let basename = Filename.chop_extension Sys.argv.(0) in
  let basename =
    let suffix = "_with_lines" in
    let nsuffix = String.length suffix in
    let nbasename = String.length basename in
    if nbasename >= nsuffix && (String.sub basename (nbasename - nsuffix) nsuffix) = suffix
      then String.sub basename 0 (nbasename - nsuffix)
      else basename
      in
  (* Set the input file, execute the function [f], dump the results. *)
  script (fun () ->
    Trace.init (basename ^ ".cpp");
    f();
    flush stdout;
    Trace.dump ~prefix ();
  )


(******************************************************************************)
(*                              DEPRECATED                                    *)
(******************************************************************************)



(* DEPRECATED but keep for future use?
  let fwrapper : unit -> unit =
    (fun () ->
      try f (); close_logs () with
      | Failure s ->
         close_logs ();
         failwith s
    )
  in
  if !Flags.repeat_io then fwrapper ()
  else
    (*FANCY
   try script () with
    | _ ->
       (*
         if an error occurs, restart with printing/parsing at each step to have
         appropriate location computation and error messages
        *)
       print_info None "Error while executing the transformation script.\n";
       print_info None "Restarting with printing/parsing at each step...\n";
       Flags.repeat_io := true;
       reset ();
       script ()
     *)
    fwrapper()
*)


(*
  split the sequence(s) around the instruction(s) pointed by tr
  property: the result sequence is of the form
    result_label:
    {var (split_name x0) decl
    …
    var (split_name xn) decl
    block1_label: {block 1; split_name x0 = x0; …; split_name xn = xn}
    block2 label:
    {var x0 decl = split_name x0; …; var xn decl = split_name xn; block 2}}
  where x0, …, xn are the vars declared in block1 that are used in block 2
  labels are defined from the labels argument:
    - labels = [] -> (result, result_block1, result_block2)
    - labels = [s] -> (s, s_block1, s_block2)
    - labels = [l1, l2, l3] -> (l1, l2, l3)
 *)
 (* TODO: Remove this later *)
(* let split_sequence ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = [])
  ?(split_name : string -> string = fun x -> x ^ "_split")
  (tr : target) : unit =
  let (result_label, block1_label, block2_label) =
    match labels with
    | [] -> ("result", "result_block1", "result_block2")
    | [s] -> (s, s ^ "_block1", s ^ "_block2")
    | [result_label; block1_label; block2_label] ->
       (result_label, block1_label, block2_label)
    | _ -> fail None "split_sequence: bad labels"
  in
  let log : string =
    let ps = target_to_string tr in
    Printf.sprintf
      ("Split_sequence %s:\n" ^^
       "  - the split_name function outputs fresh names\n"
      )
      ps
  in
  write_log log;
  apply
    (fun ctx t ->
      let t =
        Sequence.split_sequence ctx.clog result_label block1_label
          block2_label split_name tr t
      in
      if keep_labels then t else
        Label.delete_labels [result_label; block1_label; block2_label]
          t
    );
  write_log "\n" *)

(*
  extract a variable from a loop:
  - before:
    optional_label:
    for i = 0 to N / for i = N to 0
      var x
      body(x)
  - after:
    result_label:
    {
      var x[N+1]
      optional_label:
      for i = 0 to N / for i = N to 0
        body(x[i])
    }
  assumptions:
    + no initialisation for x in its declaration
    + x is heap allocated and deleted last in the loop
    + the path points to the loop
 *)
(* let extract_loop_var ?(replace_top : bool = false) ?(keep_label : bool = false)
  ?(label : string = "") (tr : target) : unit =
  let result_label = if label = "" then "result" else label in
  let log : string =
    Printf.sprintf "Extract_loop_var %s:\n" (target_to_string tr)
  in
  write_log log;
  apply
    (fun ctx t ->
      let t = Loop.extract_loop_var ctx.clog result_label tr t in
      if keep_label then t else Label.delete_label result_label t
    );
  write_log "\n"

let extract_loop_vars ?(replace_top : bool = false) ?(keep_label : bool = false)
  ?(label : string = "") (tr : target) : unit =
  let result_label = if label = "" then "result" else label in
  let log : string =
    Printf.sprintf "Extract_loop_vars %s:\n" (target_to_string tr)
  in
  write_log log;
  apply
    (fun ctx t ->
      let t = Loop.extract_loop_vars ctx.clog result_label tr t in
      if keep_label then t else Label.delete_label result_label t
    );
  write_log "\n" *)

(*
  split the for loop pointed by tr
  assumption: the loop is of the form
  optional_label:
  for i = 0 to N / for i = N to 0
    {block1}
    {block2}
  where block1 and block2 are independent
  result:
    result_label:{
      loop1_label:
      for i = 0 to N / for i = N to 0
        block1
      loop2_label:
      for i = 0 to N / for i = N to 0
        block2
    }
  labels are defined from the labels argument:
    - labels = [] -> (result, result_loop1, result_loop2)
    - labels = [s] -> (s, s_loop1, s_loop2)
    - labels = [l1, l2, l3] -> (l1, l2, l3)
 *)
(* let split_loop_nodep ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = []) (tr : target) : unit =
  let (result_label, loop1_label, loop2_label) =
    match labels with
    | [] -> ("result", "result_loop1", "result_loop2")
    | [s] -> (s, s ^ "_loop1", s ^ "_loop2")
    | [result_label; loop1_label; loop2_label] ->
       (result_label, loop1_label, loop2_label)
    | _ -> fail None "split_loop_nodep: bad labels"
  in
  let log : string =
    Printf.sprintf "Split_loop_nodep %s:\n" (target_to_string tr)
  in
  write_log log;
  apply
    (fun ctx t ->
      let t =
        Loop.split_loop_nodep ctx.clog result_label loop1_label
          loop2_label tr t
      in
      if keep_labels then t else
        Label.delete_labels [result_label; loop1_label; loop2_label] t
    );
  write_log "\n" *)

(* TODO: When implemented in combi, remove it *)
(*
  combine split_sequence, extract_loop_var and split_loop_nodep to split the for
  loop after the instruction pointed by tr in t
 *)
(* let split_loop ?(replace_top : bool = false) ?(keep_labels : bool = false)
  ?(labels : string list = [])
  ?(split_name : string -> string = fun x -> x ^ "_split")
  (tr : target) : unit =
  let (result_label, loop1_label, loop2_label) =
    match labels with
    | [] -> ("result", "result_loop1", "result_loop2")
    | [s] -> (s, s ^ "_loop1", s ^ "_loop2")
    | [result_label; loop1_label; loop2_label] ->
       (result_label, loop1_label, loop2_label)
    | _ -> fail None "split_loop: bad labels"
  in
  let (log_begin, log_end) : string * string =
    let ps = target_to_string tr in
    (Printf.sprintf "##### Start of split_loop %s #####\n\n" ps,
     Printf.sprintf "##### End of split_loop %s #####\n\n" ps)
  in
  write_log log_begin;
  (* split the loop body *)
  split_sequence
    ~keep_labels:true
    ~labels:["split_loop_tmp_result"; "split_loop_tmp_block1";
             "split_loop_tmp_block2"]
    ~split_name
    tr;
  (* make sure at most one ast is added to the stack *)
  let replace_top = true in
  (* label the loop for later calls to Generic *)
  Label.add "split_loop_tmp_loop"
    [cFor ~body:[cLabel "split_loop_tmp_result"
                   ~substr:true ] ""];
  (* remove unnecessary labels *)
  delete_labels ["split_loop_tmp_result"; "split_loop_tmp_block1";
                              "split_loop_tmp_block2"];
  let tr' = [cLabel "split_loop_tmp_loop" ~substr:true] in
  (* extract loop variables *)
  extract_loop_vars ~keep_label:true ~label:result_label tr';
  (* split the loop *)
  split_loop_nodep ~keep_labels:true
    ~labels:["split_loop_tmp_result"; loop1_label; loop2_label] tr';
  (* clean up *)
  delete_labels ["split_loop_tmp_result"; "split_loop_tmp_loop"];
  if not keep_labels then
    delete_labels [result_label; loop1_label; loop2_label];
  write_log log_end *)


(* let move_loop_before ?(replace_top : bool = false) (tr : target) (loop_index : var) : unit =
    let log : string =
      Printf.sprintf "move_loop_before %s:\n" (target_to_string tr)
    in
    write_log log;
    apply
      (fun ctx -> Loop.move_loop_before ctx.clog tr loop_index);
    write_log "\n"

let move_loop_after ?(replace_top : bool = false) (tr : target) (loop_index : var) : unit =
    let log : string =
      Printf.sprintf "move_loop_after %s:\n" (target_to_string tr)
    in
    write_log log;
    apply
      (fun ctx -> Loop.move_loop_after ctx.clog tr loop_index);
    write_log "\n"

let move_loop ?(replace_top : bool = false) ?(move_before : string  = "") ?(move_after : string = "" ) (loop_index : string) : unit =
    apply
      (fun ctx -> Loop.move_loop ctx.clog  ~move_before ~move_after loop_index);
    write_log "\n" *)

(* let inline_record_access ?(replace_top : bool = false) ?(field : string = "") ?(var : string = "") () : unit =
  apply
    (fun ctx -> Inlining.inline_record_access ctx.clog  field var);
  write_log "\n" *)

(* let rewrite ?(replace_top : bool = false) ?(rule : string = "") ?(path : target = [ ]) : () : unit =
  apply
    (fun ctx -> Generic.rewrite ctx.clog rule path );
  write_log "\n"
*)

