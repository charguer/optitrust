open Ast

(* [bind_intro ~fresh_name ~constr tg]  expects tg to point to a function call.
        Then it will generate a new variable declaration named as [fresh_name]
        and with an initial value equal to the trm targeted by [tg]. If [const] is
        true then the binded variable will be declared as a immutable variable otherwise immutable.
        Finally It will replace the targeted term with the binded variable.
      Example: let suppose that the target is g(x) then for the following example we will have
        {                         {
          int x = 3;                int x = 3;
          int y = f(g(x));          const int a = g(x);
        }                           int y = f(a);
                                  }

*)
(* let bind_intro ?(fresh_name : var = "__OPTITRUST___VAR") ?(const : bool = true) ?(my_mark : mark = "") (tg : Target.target) : unit =
 Target.apply_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
  (fun (p, p_local, i) t ->  Function_core.bind_intro ~my_mark i fresh_name const p_local t p) tg *)
let bind_intro ?(fresh_name : var = "__OPTITRUST___VAR") ?(const : bool = true) ?(my_mark : mark = "") (tg : Target.target) : unit =
  Target.applyi_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
    (fun occ t (p, p_local, i)  ->
      let fresh_name = Str.global_replace (Str.regexp_string "${occ}") (string_of_int occ) fresh_name  in
    Function_core.bind_intro ~my_mark i fresh_name const p_local t p) tg


(* [inline ~body_mark tg] - expects the target [tg] to point to a function call inside a declaration
    or inside a sequence in case the function is of void type. Example:
          int r = g(a);
      or  g(a);

    Then it will replace that instruction with a nobrace sequence which is a sequence
    hidden from the user. This sequence will be marked with [body_mark] and it will
    contain the body of the declaration of the called function targeted with [tg].
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
*)


let inline ?(body_mark : var = "body") (tg : Target.target) : unit =
  Internal.nobrace_remove_after (fun _ ->
  Target.apply_on_transformed_targets (Internal.get_instruction_in_surrounding_sequence)
   (fun  t (p, p_local, i) ->
    Function_core.inline i body_mark t p_local t p) tg)

(* [beta ~body_mark tg] its the same as function_inline *)
let beta  ?(body_mark : var = "body") (tg : Target.target) : unit =
  inline ~body_mark tg

let use_infix_ops ?(tg_ops : Target.target = [Target.nbMulti;Target.cWrite ~rhs:[Target.cPrimPredFun is_infix_prim_fun] ()]) () : unit =
  Target.apply_on_targets (Function_core.use_infix_ops) tg_ops

let uninline ~fct (*:Target.target*) (tg : Target.target) : unit =
  Trace.call (fun t ->
    let fct_path = Target.resolve_target_exactly_one fct t in
    let fct_decl = Path.resolve_path fct_path t in
    Target.apply_on_targets (Function_core.uninline fct_decl) tg)



























