open Ast
open Target

(* [delete tg]: delete the targeted function definition.
   Correct if the function is never used.
   Currently checked by verifying that the targets correspond to
   function definitions, and by retychecking the code *)
let%transfo delete (tg : target) : unit =
  let tr () =
    Sequence_basic.delete tg in
  if !Flags.check_validity then begin
    Trace.justif "The function is unused.";
    Target.iter_at_target_paths (fun t ->
      let error =  "Function.delete expects to target a function definition" in
      let _ = trm_inv ~error trm_let_fun_inv t in
      ()
    ) tg;
    tr();
    Trace.retypecheck(); (* TODO: report error in unit test *)
  end else
    tr ()


(* [bind_intro ~fresh_name ~const ~my_mark tg]: expects the target [t] to point at a function call.
     Then it will generate a new variable declaration named as [fresh_name] with type being the same
     as the one of the function call, and initialized to the function call itself.
     If [const] is true the the binded variable will be declraed as an immutable variable otherwise immutable.
     Then it will fold the newly declared variable.

     @correctness: correct if the new order of evaluation of expressions is
      not changed or does not matter. *)
let%transfo bind_intro ?(fresh_name : var = "__OPTITRUST___VAR") ?(const : bool = true) ?(my_mark : mark = "") (tg : target) : unit =
  Internal.nobrace_remove_after ( fun _ ->
    applyi_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun occ t (p, p_local, i)  ->
      let fresh_name = Tools.string_subst "${occ}" (string_of_int occ) fresh_name in
    Function_core.bind_intro ~my_mark i fresh_name const p_local t p) tg
  )

(* [inline ~body_mark tg]: expects the target [tg] to point at a function call inside a declaration
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

let%transfo inline ?(body_mark : mark option) ?(subst_mark : mark option) (tg : target) : unit =
  Trace.justif "Function inlining is always correct (exploiting the fact that arguments are duplicable expressions).";
  Internal.nobrace_remove_after (fun _ ->
    Stats.comp_stats "inline apply_on_transformed_targets" (fun () ->
    apply_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
     (fun  t (p, p_local, i) ->
        Stats.comp_stats "inline call to Function_core.inline" (fun () ->
          Function_core.inline i body_mark ~subst_mark p_local t p)) tg))


(* [beta ~body_mark tg]: similar to [function_inline] the main difference is that [beta] is used in the cases
    when the decaration of the function call can be founded at the targeted function call contrary to [inline]
    which will need to find first the toplevel declaration.  *)
let beta ?(body_mark : var = "") (tg : target) : unit =
  inline ~body_mark tg


(* [use_infix_ops_at tg]: expects the target [tg] to point at an explicit set operation of the form x = x (op) a,
    then it will transform that instruction into x (op)= a. Ex: x = x + 1 --> x += 1. *)
let%transfo use_infix_ops_at ?(allow_identity : bool = true) (tg : target) : unit =
  apply_on_targets (Function_core.use_infix_ops allow_identity) tg

(* [uninline ~fct tg] expects the target [ŧg] to be pointing at a labelled sequence similar to what Function_basic.inline generates
    Then it will replace that sequence with a call to the fuction with declaration targeted by [fct]. *)
let%transfo uninline ~fct:(fct : target) (tg : target) : unit =
  Trace.call (fun t ->
    let fct_path = resolve_target_exactly_one_with_stringreprs_available fct t in
    let fct_decl = Path.resolve_path fct_path t in
    apply_on_targets (Function_core.uninline fct_decl) tg)

(* [rename_args new_args tg]: expects the target [tg] to point at a function declaration, then it will rename the args of
     that function. If there are local variables declared inside the body of the function that have the same name as one
     of the function args then it will skip those variables on all their occurrences. *)
let%transfo rename_args (new_args : var list) (tg : target) : unit =
  apply_on_targets (Function_core.rename_args new_args) tg


(* [replace_with_change_args new_fun_name arg_mapper tg]: expects the target [tg] to point at a function call, then it will
    replace the name of the called function with [new_fun_name] and apply [arrg_mapper] to its arguments. *)
let%transfo replace_with_change_args (new_fun_name : string) (arg_mapper : trms -> trms) (tg : target) : unit =
   apply_on_targets (Function_core.replace_with_change_args new_fun_name arg_mapper) tg

(* [dsp_def ~arg ~func tg]: expects the target [tg] to point at a function definition, then it will
     inserts a new version of that definition whose return type is void.
    [arg] - is the name of the argument that's going to be inserted,
    [func] - the name of the new function that's going to be inserted. *)
let%transfo dsp_def ?(arg : var = "res") ?(func : var = "dsp") (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Function_core.dsp_def i arg func t p) tg)

(* [dsp_call ~dsp tg]: expects the target [tg] to point at a function call whose parent trm is a write operation
    then it will convert that write operation into a function call.
    Let's say that the targeted function call is r = f(x, y);
    If [dsp] is the empty string, then "f_dsp" will be used as a name based on the original name "f".
    Note: This transformation assumes that dsp_def has been already applied to the definition of the called function. *)
let%transfo dsp_call ?(dsp : var = "") (tg : target) : unit =
  apply_on_transformed_targets (Path.parent)
    (fun t p -> Function_core.dsp_call dsp t p) tg