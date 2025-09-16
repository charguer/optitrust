open Prelude
open Target

(** [delete tg]: delete the targeted function definition.
   Correct if the function is never used.

   Currently checked by verifying that the targets correspond to
   function definitions, and by retychecking the code *)
let%transfo delete (tg : target) : unit =
  let tr () =
    Sequence_basic.delete tg in
  if !Flags.check_validity then begin
    Target.iter (fun p ->
      let error =  "Function.delete expects to target a function definition within a sequence" in
      let (_, _, _, _, _) = trm_inv ~error trm_let_fun_inv (resolve_path p) in
      Scope.justif_unused p
    ) tg;
    tr();
    (* Trace.retypecheck(); (* TODO: report error in unit test *) *)
  end else
    tr ()

(** [inline ~body_mark tg]: expects the target [tg] to point at a function call inside a declaration
    or inside a sequence in case the function is of void type. Example:
          int r = g(a);
      or  g(a);

    Then it will replace that instruction with a nobrace sequence which is a sequence visible on the ast level.
    This sequence will be marked with [body_mark] and it will contain the body of the declaration of the called
    function targeted with [tg].
    Transformation steps:
       1) generate in that sequence the binding "int r", in case it is needed
          (if the original instructions featured a "int r = ..")

       2) replacing the name of the arguments with the expressions that were
           provided to the call.

          - Instructions of the form "return foo;" should be translated into
              "r = foo; goto __exit_body;"
          - Instructions of the form "return;" should be translated into
              "goto __exit_body;"
          - The "goto" is not needed in case the instruction is the "last one"
              of the body. To keep track of this, the recursive traversal function
              maintains a boolean flag "islast". This flag is true initially, but
              becomes false as soon as one enters the branch of a Trm_seq that is
            not the last branch. (see examples further below).
            - You can use a reference to save the information of whether at least
              one "goto" operation was generated.

        3) generate the exit label ("__exit_" ^ label) in case we observed the need
          for a goto during the translation of the body

     Example:

      int g(int x) {
        int y = x + x;
        return y + y;
      }
      int r = [target:]g(a)

     this result is:

        @nobrace{
          int r;
          body: {
             int y = a + a;
             r = y + y;
          }
        }

   @correctness: always works, and also needs to instantiate variables in the
   local invariants in the body. *)

let%transfo inline ?(body_mark : mark = no_mark) ?(subst_mark : mark = no_mark) (tg : target) : unit =
  Target.apply_at_target_paths (Function_core.inline_on ~body_mark ~subst_mark) tg

(** [beta ~body_mark tg]: similar to [function_inline] the main difference is that [beta] is used in the cases
    when the decaration of the function call can be founded at the targeted function call contrary to [inline]
    which will need to find first the toplevel declaration.  *)
let beta ?(body_mark : mark = no_mark) (tg : target) : unit =
  Target.apply_at_target_paths (Function_core.beta_reduce_on ~body_mark) tg


(** [use_infix_ops_at tg]: expects the target [tg] to point at an explicit set operation of the form x = x (op) a,
    then it will transform that instruction into x (op)= a. Ex: x = x + 1 --> x += 1. *)
let%transfo use_infix_ops_at ?(allow_identity : bool = true) (tg : target) : unit =
  apply_at_target_paths (Function_core.use_infix_ops_on allow_identity) tg

(** [uninline ~f tg] expects [tg] to target a span of instructions.
    Then it will replace that span with a call to the fuction with declaration targeted by [f].
    The annotations are deleted both in the span and in the function declaration for matching *)
let%transfo uninline ~(f : target) (tg : target) : unit =
  Trace.call (fun t ->
  let f_path = resolve_target_exactly_one_with_stringreprs_available f t in
  let f_decl = Path.resolve_path f_path t in
  let f_decl = Nobrace.remove_after_trm_op Resource_trm.delete_annots_on f_decl in
  Target.iter (fun p ->
    let (p_seq, span) = Path.extract_last_dir_span p in
    let to_type_ret_t = ref None in
    Target.apply_at_path (Function_core.uninline_on f_decl to_type_ret_t span) p_seq;
    Option.iter (fun to_type_t ->
      (* DEPRECATED: is it really a problem to alias arguments with return address? *)
      step_backtrack ~discard_after:false (fun () ->
        Target.apply_at_path (fun t_seq ->
          update_span_helper span t_seq (fun _ -> to_type_t)
        ) p_seq
      )
    ) !to_type_ret_t;
  ) tg)

(** [rename_args new_args tg]: expects the target [tg] to point at a function declaration, then it will rename the args of
     that function. If there are local variables declared inside the body of the function that have the same name as one
     of the function args then it will skip those variables on all their occurrences. *)
let%transfo rename_args (new_args : var list) (tg : target) : unit =
  apply_at_target_paths (Function_core.rename_args_on new_args) tg


(** [replace_with_change_args new_fun_name arg_mapper tg]: expects the target [tg] to point at a function call, then it will
    replace the name of the called function with [new_fun_name] and apply [arrg_mapper] to its arguments. *)
let%transfo replace_with_change_args (new_fun_name : var) (arg_mapper : trms -> trms) (tg : target) : unit =
  apply_at_target_paths (Function_core.replace_with_change_args_on new_fun_name arg_mapper) tg

(** [dps_def ~arg ~func tg]: expects the target [tg] to point at a function definition, then it will
     inserts a new version of that definition whose return type is void.
    [arg] - is the name of the argument that's going to be inserted,
    [func] - the name of the new function that's going to be inserted. *)
let%transfo dps_def ?(arg : string = "res") ?(fn_name: string = "") (tg : target) : unit =
  Nobrace_transfo.remove_after (fun _ ->
    apply_at_target_paths_in_seq (fun i t -> Function_core.dps_def_at i arg ~fn_name t) tg)

(** [dps_call ~dps tg]: expects the target [tg] to point at a function call whose parent trm is a write operation
    then it will convert that write operation into a function call.
    Let's say that the targeted function call is r = f(x, y);
    If [dps] is the empty string, then "f_dps" will be used as a name based on the original name "f".
    Note: This transformation assumes that dps_def has been already applied to the definition of the called function. *)
let%transfo dps_call ?(dps : string = "") (tg : target) : unit =
  Target.iter (fun p ->
    Target.apply_at_path (Function_core.dps_call_on dps) (Path.parent p)
  ) tg;
  Scope.infer_var_ids ()
