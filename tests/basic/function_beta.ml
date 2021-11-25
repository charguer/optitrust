(*

beta reduction  is just a form of inlining, where the function is defined "on-the-fly".

  in inlining you have

---
  void f(int j) {
      s += 2*j;
      s -= j;
  }
  int main() {
    int i = 1;
    f(i)
  }
---

  whereas in beta you have:

---
    (void f(int j) {
      s += 2*j;
      s -= j;
    }))(i);
---

  that is, a trm_app with the base being a trm_let_fun.


  The transformation Function_basic.beta takes as target the trm_app node.
    Function_basic.beta = Function_basic.inline

  The combi transformation Function.beta takes a target:

  Function.beta =
    - if this target points to a trm_app, apply Function_basic.beta on it
    - if this target points to a trm_let_fun, check that the parent node is a trm_app, and target this one.
        int a = (void f(int x) { return x; })(3)
        ->
        int a =3


        same result as if you ave
          f(3)

        trm_app ~base:[trm_let_fun ~name:"f"]
        [trm_let_fun ~name:"f"]


  The point is that the user can say "beta reduce the function f"

  The combi transformation has prototype:
      Function.beta ?(target:target=[]) ()
  when target is not provided, we use the target [cApp ~base:[cFunDef()]]
  to indicate that we are looking for any application of a function definition through the AST.

*)

open Optitrust
open Target

let _ = Run.script_cpp (fun _ ->

  !! Function_basic.beta [cFun "f"];

)



