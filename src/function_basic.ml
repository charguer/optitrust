open Ast

(* [bind_intro ~fresh_name ~constr tg]  expects tg to point to a function call.
        Then it will generate a new variable declaration with name [fresh_name]
        with initialized value equal to the trm targeted by [tg]. If [const] is
        true then the binded variable will be declared as mutable otherwise immutable.
        Finally It will replace the targeted term with the binded variable.
      Example: let suposse that the target is g(x) then for the following example we will have
        {                         {
          int x = 3;                int x = 3;
          int y = f(g(x));          const int a = g(x);
        }                           int y = f(a);
                                  }

*)
let bind_intro ?(fresh_name : var = "a") ?(const : bool = true) : Target.Transfo.t =
 Target.apply_on_transformed_targets (Internal.get_call_in_surrounding_sequence)
  (fun (p, p_local, i) t ->  Function_core.bind_intro i fresh_name const p_local t p)


(* TODO: update / complete the spec below *)
(* [inline_call ~label tg] - expects the target [tg] to point to a function call.
    Then it will replace that instruction with a nobrace sequence which is a sequence
    visible only inside the ast. This sequence will be labelled with [label]. Basically
    this sequence contains the body of the declaration of the called function targeted with
    [tg]. This transformation end with some tunnings of the copied body listed below:

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

     this result is:

        @nobrace{
          int r;
          body: {
             int y = a + a;
             r = y + y;
          }
        }
*)
let inline_call  ?(label : var = "body") : Target.Transfo.t =
  Target.apply_on_transformed_targets (Internal.get_call_in_surrounding_sequence)
   (fun (p, p_local, i) t ->
    Function_core.inline_call i label t p_local t p)

