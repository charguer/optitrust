open Ast
open Target

(* [bind_intro ~fresh_name ~const ~my_mark tg]: expects the target [t] to point at a function call.
     Then it will generate a new variable declaration named as [fresh_name] with type being the same
     as the one of the function call, and initialized to the function call itself.
     If [const] is true the the binded variable will be declraed as an immutable variable otherwise immutable.
     Then it will fold the newly declared variable. 
     
     @correctness: correct if the new order of evaluation of expressions is
      not changed or does not matter. *)
let bind_intro ?(fresh_name : var = "__OPTITRUST___VAR") ?(const : bool = true) ?(my_mark : mark = "") (tg : target) : unit =
  applyi_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun occ t (p, p_local, i)  ->
      let fresh_name = Tools.string_subst "${occ}" (string_of_int occ) fresh_name in
    Function_core.bind_intro ~my_mark i fresh_name const p_local t p) tg

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

   @correctness: always work, and also need to instantiate variables in the
   local invariants in the body. *)

let inline ?(body_mark : mark option) (tg : target) : unit =
  Internal.nobrace_remove_after (fun _ ->
    Trace.time "inline apply_on_transformed_targets" (fun () ->
    apply_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
     (fun  t (p, p_local, i) ->
        Trace.time "inline call to Function_core.inline" (fun () ->
          Function_core.inline i body_mark p_local t p)) tg))


(* [beta ~body_mark tg]: similar to [function_inline] the main difference is that [beta] is used in the cases 
    when the decaration of the function call can be founded at the targeted function call contrary to [inline]
    which will need to find first the toplevel declaration.  *)
let beta ?(body_mark : var = "") (tg : target) : unit =
  inline ~body_mark tg


(* [use_infix_ops_at tg]: expects the target [tg] to point at an explicit set operation of the form x = x (op) a,
    then it will transform that instruction into x (op)= a. Ex: x = x + 1 --> x += 1. *)
let use_infix_ops_at ?(allow_identity : bool = true) : Transfo.t =
  apply_on_targets (Function_core.use_infix_ops allow_identity)

(* [uninline ~fct tg] expects the target [ŧg] to be pointing at a labelled sequence similar to what Function_basic.inline generates
    Then it will replace that sequence with a call to the fuction with declaration targeted by [fct]. *)
let uninline ~fct:(fct : target) (tg : target) : unit =
  Trace.call (fun t ->
    let fct_path = resolve_target_exactly_one_with_stringreprs_available fct t in
    let fct_decl = Path.resolve_path fct_path t in
    apply_on_targets (Function_core.uninline fct_decl) tg)

(* [rename_args new_args tg]: expects the target [tg] to point at a function declaration, then it will rename the args of 
     that function. If there are local variables declared inside the body of the function that have the same name as one 
     of the function args then it will skip those variables on all their occurrences. *)
let rename_args (new_args : var list)  : Transfo.t =
  apply_on_targets (Function_core.rename_args new_args)



(* [replace_with_change_args new_fun_name arg_mapper tg]: expects the target [tg] to point at a function call, then it will
    replace the name of the called function with [new_fun_name] and apply [arrg_mapper] to its arguments. *)
let replace_with_change_args (new_fun_name : string) (arg_mapper : trms -> trms) (tg : target) : unit = 
   apply_on_targets (Function_core.replace_with_change_args new_fun_name arg_mapper) tg

(* [dsp_def ~arg ~func tg] *)
let  dsp_def ?(arg : var = "res") ?(func : var = "dsp") : Transfo.t = 
  apply_on_transformed_targets (Internal.isolate_last_dir_in_seq)
    (fun t (p,i) -> Function_core.dsp_def i arg func t p)
