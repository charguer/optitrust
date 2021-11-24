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

  The combi transformation Function.beta takes a target:
    - if this target points to a trm_app, apply Function_basic.beta on it
    - if this target points to a trm_let_fun, check that the parent node is a trm_app, and target this one.
  The point is that the user can say "beta reduce the function f"

  The combi transformation has prototype:
      Function.beta ?(target:target=[]) ()
  when target is not provided, we use the target [cApp ~base:[cFunDef()]]
  to indicate that we are looking for any application of a function definition through the AST.

*)

