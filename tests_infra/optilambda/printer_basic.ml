open Optitrust_ast
open Ast

module OL = Optitrust_optilambda.Optilambda
open Optitrust_optilambda.Optilambda_style

let v name =
  Ast.name_to_var name

let tv name ty =
  v name, ty

let term name =
  Trm.trm_var (v name)

let resource_set ?(pure = []) ?(linear = []) () =
  { empty_resource_set with pure; linear }

let simple_fun_contract =
  {
    pre = resource_set
      ~pure:[v "h_req", Trm.trm_eq ~typ:Typ.typ_int (term "x") (term "y")]
      ~linear:[v "h_in", term "R"]
      ();
    post = resource_set
      ~pure:[v "h_ens", Trm.trm_eq ~typ:Typ.typ_int (term "result") (term "x")]
      ~linear:[v "h_out", term "R2"]
      ();
  }

let simple_loop_contract =
  {
    empty_loop_contract with
    loop_ghosts = [v "h_loop", Trm.trm_lt ~typ:Typ.typ_int (term "i") (term "n")];
    invariant = resource_set ~pure:[v "h_inv", Trm.trm_le ~typ:Typ.typ_int (Trm.trm_int 0) (term "i")] ();
    iter_contract = {
      pre = resource_set ~pure:[v "h_xreq", Trm.trm_lt ~typ:Typ.typ_int (term "i") (term "n")] ();
      post = resource_set ~linear:[v "h_xprod", term "Done"] ();
    };
  }

let ghost_call_example =
  Trm.trm_ghost_force
    (Trm.ghost_call
       ~ghost_bind:[Some (v "z"), "h_out"]
       (v "rewrite")
       ["h", Trm.trm_eq ~typ:Typ.typ_int (term "x") (term "y")])

let check name trm expected =
  let actual = OL.trm_to_string trm in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda printer test failed: %s\nexpected:\n%s\nactual:\n%s\n"
      name expected actual;
    exit 1
  end

let check_with_style name style trm expected =
  let actual = OL.trm_to_string ~style trm in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda printer test failed: %s\nexpected:\n%s\nactual:\n%s\n"
      name expected actual;
    exit 1
  end

let () =
  check "int literal"
    (Trm.trm_int 3)
    "3";

  check "variable"
    (term "x")
    "x";

  check "let"
    (Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3))
    "let x: int = 3";

  check "letmut"
    (Trm.trm_let (tv "x" (Typ.typ_ptr Typ.typ_int))
       (Trm.trm_ref Typ.typ_int (Trm.trm_int 3)))
    "letmut x = 3";

  check "call"
    (Trm.trm_apps (term "f") [term "x"; Trm.trm_int 1])
    "f(x, 1)";

  check "assignment"
    (Trm.trm_set (term "x") (Trm.trm_int 4))
    "x = 4";

  check "array access"
    (Trm.trm_array_get (term "t") (term "i"))
    "t[i]";

  check "precedence: add inside mul"
    (Trm.trm_mul ~typ:Typ.typ_int
       (Trm.trm_add ~typ:Typ.typ_int (term "x") (term "y"))
       (term "z"))
    "(x + y) * z";

  check "precedence: mul inside add"
    (Trm.trm_add ~typ:Typ.typ_int
       (term "x")
       (Trm.trm_mul ~typ:Typ.typ_int (term "y") (term "z")))
    "x + y * z";

  check "contract call suffix"
    (Trm.trm_apps
       ~ghost_args:[v "h1", Trm.trm_int 3]
       ~ghost_bind:[Some (v "z"), v "h2"]
       (term "f")
       [term "x"])
    "f(x)[h1 := 3][z : h2]";

  check "function definition"
    (Trm.trm_let_fun
       (v "f")
       Typ.typ_int
       [tv "x" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]))
    "fun f(x: int): int { return x }";

  check "function contract"
    (Trm.trm_let_fun
       ~contract:(FunSpecContract simple_fun_contract)
       (v "f")
       Typ.typ_int
       [tv "x" Typ.typ_int; tv "y" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]))
    "fun f(x: int, y: int): int [h_req, h_in, h_ens, h_out] {\n  requires h_req: x = y;\n  consumes h_in: R;\n  ensures h_ens: result = x;\n  produces h_out: R2;\n  return x\n}";

  check "if"
    (Trm.trm_if
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n"))
       (Trm.trm_seq_nomarks [Trm.trm_set (term "x") (Trm.trm_int 1)])
       (Trm.trm_seq_nomarks [Trm.trm_set (term "x") (Trm.trm_int 0)]))
    "if (x < n) { x = 1 } else { x = 0 }";

  check "while"
    (Trm.trm_while
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n"))
       (Trm.trm_seq_nomarks [
          Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1));
        ]))
    "while (x < n) { x = x + 1 }";

  check "for"
    (Trm.trm_for
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [
          Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1));
        ]))
    "for<seq, up>(i = 0, n, 1) { x = x + 1 }";

  check "loop contract"
    (Trm.trm_for
       ~contract:simple_loop_contract
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [
          Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1));
        ]))
    "for<seq, up>(i = 0, n, 1) [h_loop, h_inv, h_xreq, h_xprod] {\n  requires h_loop: i < n;\n  requires h_inv: 0 <= i;\n  xrequires h_xreq: i < n;\n  xproduces h_xprod: Done;\n  x = x + 1\n}";

  check "ghost call"
    ghost_call_example
    "ghost(rewrite()[h := x = y][z : h_out])";

  check_with_style "style hides types"
    { OL.default_style with print_types = false }
    (Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3))
    "let x = 3";

  check_with_style "style omits default loop step"
    { OL.default_style with omit_default_loop_step = true }
    (Trm.trm_for
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [
          Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1));
        ]))
    "for<seq, up>(i = 0, n) { x = x + 1 }";

  check_with_style "style hides contracts"
    { OL.default_style with print_contracts = false }
    (Trm.trm_let_fun
       ~contract:(FunSpecContract simple_fun_contract)
       (v "f")
       Typ.typ_int
       [tv "x" Typ.typ_int; tv "y" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]))
    "fun f(x: int, y: int): int { return x }";

  check "marks"
    (Mark.trm_add_mark "target" (term "x"))
    "@marks[target] x"
