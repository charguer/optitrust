
open Syntax
open Target
open Path
open Mlist
include Apac_basic
include Apac_core

(* [constify tg]: expects target [tg] to point at a function definition. It
   constifies function arguments and functions whenever it is possible depending
   on data accesses, aliases and dependencies. *)
let constify (tg : target) : unit =
  (* Step 1: Clear the hash table of constification records. *)
  Var_Hashtbl.clear Apac_core.const_records;
  (* Step 2: Iterate over [tg] and fill the hash table of constification
     records. *)
  Apac_core.build_constification_records tg;
  (* Step 3: Clear the stack of arguments and functions to unconstify after the
     analysis of data accesses, aliases and dependencies. Indeed, the
     constification algorithm begins by consedring that all functions and
     function arguments can be constified. It then performs an analysis (in
     Step 4) of data accesses, aliases and dependencies and unconstifies all of
     the variables (aliases), arguments and functions where the constification
     is not possible. *)
  Stack.clear Apac_core.to_unconst;
  (* Step 4: Perform an analysis of data accesses, aliases and dependencies,
     then decide which variables (aliases), arguments and functions should not
     be constified. *)
  Apac_core.identify_mutables tg;
  (* Step 5: Propagate the unconstification. *)
  Apac_core.unconstify_mutables ();
  (* Step 6: Effectively transform the AST so as to add 'const' keywords to
     function arguments, functions and *)
  Apac_basic.constify_args tg;
  (* Step 7: aliases of function arguments. Here, we begin by clearing the
     global hash table of multiple variable declarations that are subject to
     partial constification, i.e. one or more of the underlying variable
     declarations should be constified. *)
  Hashtbl.clear Apac_core.to_const_mult;
  (* We then perform the constification of aliases. However, this transformation
     cannot perform partial constification. In such cases, it only collects the
     necessary information about the concerned multiple variable declarations
     and inserts it into [Apac_core.to_const_mult]. *)
  Apac_basic.constify_aliases tg;
  (* At the end, we iterate over the key-value pairs of
     [Apac_core.to_const_mult], unfold the concerned multiple variable
     declarations having a specific mark (see [Apac_core.const_mult] and the
     comments in [Apac_basic.constify_arguments]) into sequences of simple
     variable declarations and constify those declaration which should be
     constified. *)
  Hashtbl.iter (fun k v ->
      Apac_basic.unfold_let_mult ~constify:v (tg @ [cMark k])
    ) Apac_core.to_const_mult

(* [unfold_function_calls tg]: expects target [tg] to point at a function
   definition. It moves all function calls under target [tg] out of variable
   declarations and nested function calls.

    Example:

          int a = f(g(2));

    becomes:

          int __var_1;
          __var_1 = g(2);
          int __var_2;
          __var_2 = f(__var_1);
          int a = __var_2;

    However:

          int a;
          a = f(g(2));

    becomes:

          int a;
          int __var_1;
          __var_1 = g(2);
          a = f(__var_1);

    as the call to 'f' is already dissociated from the declaration of 'a'. See
    also comments within the function.
*)
let unfold_function_calls (tg : target) : unit =
  Target.iter (fun t p ->
    (* Get the parent term to check whether it is an assignment (outside of a
       declaration). If it is the case, we do not need to apply the
       transformation. It would only create a superfluous variable. *)
    let parent_path = Path.parent p in
    let parent_target = target_of_path parent_path in
    if not (is_set_operation (get_trm_at_exn parent_target))
    then begin
      (* Define new intermediate variable. *)
      let var = fresh_var_name ~prefix:"__var_" () in
      (* Bind the return value of the current function call to that variable. *)
      Variable_basic.bind var (target_of_path p);
      (* Separate the assignment of the return value from the declaration of the
         variable. *)
      Variable_basic.init_detach [cVarDef var];
    end
  ) tg

(* [parallel_task_group tg]: expects target [tg] to point at a function
    definition.

    The first step of the transformation consists in replacing return statements
    by gotos. At the beginning of the process, the function's body is wrapped
    into a sequence to which a mark is assigned.
    See [Apac_basic.use_goto_for_return] for more details.

    In the second step, we put the marked sequence into an OpenMP task group.
    See [Apac_basic.task_group] for more details. *)
let parallel_task_group : Transfo.t =
  Target.iter (fun t p ->
    (* Create a mark. *)
    let mark = Mark.next() in
    (* Wrap the target function's body into a marked sequence and replace return
       statements by gotos. *)
    Apac_basic.use_goto_for_return ~mark (target_of_path p);
    (* Get the name of the target function through the deconstruction of the
       corresponding AST term. *)
    let error =
    "Apac.parallel_task_group: expected a target to a function definition" in
    let (qvar, _, _, _) = trm_inv ~error trm_let_fun_inv (
      Path.get_trm_at_path p t
    ) in
    (* Transform the marked instruction sequence corresponding to the target
       function's body into an OpenMP task group.

       Note that if the target function is the 'main' function, we want the
       task group to be executed only by one thread, the master thread. *)
    Apac_basic.task_group ~master:(var_has_name qvar "main") [cMark mark];
    (* 5) Remove the mark. *)
    Marks.remove mark [cMark mark];
  )
