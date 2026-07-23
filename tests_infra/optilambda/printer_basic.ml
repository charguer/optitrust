open Optitrust_ast
open Ast
module OL = Optitrust_optilambda.Optilambda
open Optitrust_optilambda.Optilambda_style

(** Unit tests for the OptiLambda printer.
    The file builds small AST fragments by hand and checks their exact text,
    HTML, type, style, and representation output. *)

let v name = Ast.name_to_var name
let tv name ty = (v name, ty)
let term name = Trm.trm_var (v name)
let typed_term name ty = Trm.trm_var ~typ:ty (v name)
let app name args = Trm.trm_apps (term name) args
let resource_set ?(pure = []) ?(linear = []) () = { empty_resource_set with pure; linear }

let simple_fun_contract =
  {
    pre = resource_set ~pure:[ (v "h_req", Trm.trm_eq ~typ:Typ.typ_int (term "x") (term "y")) ] ~linear:[ (v "h_in", term "R") ] ();
    post = resource_set ~pure:[ (v "h_ens", Trm.trm_eq ~typ:Typ.typ_int (term "result") (term "x")) ] ~linear:[ (v "h_out", term "R2") ] ();
  }

let multi_requires_contract =
  {
    empty_fun_contract with
    pre =
      resource_set
        ~pure:[ (v "from", Typ.typ_int); (v "to", Typ.typ_int); (v "inside", Typ.typ_pure_fun [ (v "x", Typ.typ_int) ] Typ.typ_prop) ]
        ();
  }

let range start stop step = app "range" [ start; stop; step ]

let group_formula index range body =
  app "Group" [ range; Trm.trm_fun [ tv index Typ.typ_int ] Typ.typ_auto body ]

let read_only_formula frac body = app "_RO" [ frac; body ]

let uninit_formula body = app "Uninit" [ body ]

let points_to_formula addr resource = app "~>" [ addr; resource ]

let wand_formula required produced = app "Wand" [ required; produced ]

let surface_reads_contract =
  let frac = term "f" in
  let body = term "H" in
  {
    pre = resource_set ~pure:[ (v "f", term "_Fraction") ] ~linear:[ (v "x", read_only_formula frac body) ] ();
    post = resource_set ~linear:[ (v "x", read_only_formula frac body) ] ();
  }

let surface_writes_contract =
  let body = term "H" in
  {
    pre = resource_set ~linear:[ (v "x", uninit_formula body) ] ();
    post = resource_set ~linear:[ (v "x", body) ] ();
  }

let preserves_contract =
  {
    pre = resource_set ~linear:[ (v "ctx", term "Ctx"); (v "changed", term "Old") ] ();
    post = resource_set ~linear:[ (v "ctx", term "Ctx"); (v "changed_out", term "New") ] ();
  }

let surface_formula_contract =
  { empty_fun_contract with pre = resource_set ~linear:[ (v "h", points_to_formula (term "src") (term "H")) ] () }

let generated_name_cleanup_contract =
  let anon_hyp = Ast.new_var "" in
  let anon_binder_hyp = Ast.new_var "" in
  let anon_i = Ast.new_var "" in
  let range = range (Trm.trm_int 0) (term "n") (Trm.trm_int 1) in
  let group_body = app "H" [ Trm.trm_var anon_i ] in
  let group_body = app "Group" [ range; Trm.trm_fun [ (anon_i, Typ.typ_int) ] Typ.typ_auto group_body ] in
  {
    empty_fun_contract with
    pre = resource_set ~linear:[ (anon_hyp, term "Anon"); (v "named", term "Named"); (anon_binder_hyp, group_body) ] ();
  }

let mixed_recovery_contract =
  let frac = term "f" in
  let read_body = term "ReadH" in
  let write_body = term "WriteH" in
  {
    pre =
      resource_set
        ~pure:[ (v "f", term "_Fraction") ]
        ~linear:[ (v "read", read_only_formula frac read_body); (v "kept", term "Kept"); (v "write", uninit_formula write_body) ]
        ();
    post =
      resource_set
        ~linear:[ (v "write", write_body); (v "read", read_only_formula frac read_body); (v "new_out", term "Produced") ]
        ();
  }

let alpha_group_reads_contract =
  let frac = term "f" in
  let range_var = Ast.new_var "range" in
  let group_var = Ast.new_var "Group" in
  let h_var = Ast.new_var "H" in
  let n_var = Ast.new_var "n" in
  let pre_i = Ast.new_var "i" in
  let post_i = Ast.new_var "i" in
  let app_var fn args = Trm.trm_apps (Trm.trm_var fn) args in
  let range = app_var range_var [ Trm.trm_int 0; Trm.trm_var n_var; Trm.trm_int 1 ] in
  let group_formula index body =
    app_var group_var [ range; Trm.trm_fun [ (index, Typ.typ_int) ] Typ.typ_auto body ]
  in
  let pre_body = group_formula pre_i (app_var h_var [ Trm.trm_var pre_i ]) in
  let post_body = group_formula post_i (app_var h_var [ Trm.trm_var post_i ]) in
  {
    pre = resource_set ~pure:[ (v "f", term "_Fraction") ] ~linear:[ (v "read", read_only_formula frac pre_body) ] ();
    post = resource_set ~linear:[ (v "read", read_only_formula frac post_body) ] ();
  }

let read_only_focus_contract =
  let frac = term "f" in
  let whole = term "Whole" in
  let focused = term "Focused" in
  {
    pre = resource_set ~pure:[ (v "f", term "_Fraction") ] ~linear:[ (v "whole", read_only_formula frac whole) ] ();
    post =
      resource_set
        ~linear:
          [
            (v "wand", wand_formula (read_only_formula frac focused) (read_only_formula frac whole));
            (v "focused", read_only_formula frac focused);
          ]
        ();
  }

let simple_loop_contract =
  {
    empty_loop_contract with
    loop_ghosts = [ (v "h_loop", Trm.trm_lt ~typ:Typ.typ_int (term "i") (term "n")) ];
    invariant = resource_set ~pure:[ (v "h_inv", Trm.trm_le ~typ:Typ.typ_int (Trm.trm_int 0) (term "i")) ] ();
    iter_contract =
      {
        pre = resource_set ~pure:[ (v "h_xreq", Trm.trm_lt ~typ:Typ.typ_int (term "i") (term "n")) ] ();
        post = resource_set ~linear:[ (v "h_xprod", term "Done") ] ();
      };
  }

let ghost_call_example =
  Trm.trm_ghost_force
    (Trm.ghost_call ~ghost_bind:[ (Some (v "z"), "h_out") ] (v "rewrite") [ ("h", Trm.trm_eq ~typ:Typ.typ_int (term "x") (term "y")) ])

let arbitrary_pure_fun_ghost =
  let inner_fun_ty = Typ.typ_pure_fun [ (v "i", Typ.typ_int) ] Typ.typ_f32 in
  let fun_ty = Typ.typ_pure_fun [ (v "n", Typ.typ_int); (v "f", inner_fun_ty) ] Typ.typ_f32 in
  Trm.trm_ghost_force
    (Trm.ghost_call ~ghost_bind:[ (Some (v "reduce_sum"), "x") ] (v "assert_inhabited")
       [ ("x", app "arbitrary" [ fun_ty ]) ])

let check name trm expected =
  let actual = OL.trm_to_string trm in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda printer test failed: %s\nexpected:\n%s\nactual:\n%s\n" name expected actual;
    exit 1
  end

let check_with_style name style trm expected =
  let actual = OL.trm_to_string ~style trm in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda printer test failed: %s\nexpected:\n%s\nactual:\n%s\n" name expected actual;
    exit 1
  end

let check_typ name typ expected =
  let actual = OL.typ_to_string typ in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda type printer test failed: %s\nexpected:\n%s\nactual:\n%s\n" name expected actual;
    exit 1
  end

let check_html name trm expected =
  let actual = OL.trm_to_html trm in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda HTML printer test failed: %s\nexpected:\n%s\nactual:\n%s\n" name expected actual;
    exit 1
  end

let check_html_with_style name style trm expected =
  let actual = OL.trm_to_html ~style trm in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda HTML printer test failed: %s\nexpected:\n%s\nactual:\n%s\n" name expected actual;
    exit 1
  end

let check_repr name input expected =
  let actual =
    match representation_of_string input with
    | Some representation -> representation_to_string representation
    | None -> "<none>"
  in
  if actual <> expected then begin
    Printf.eprintf "OptiLambda representation test failed: %s\nexpected:\n%s\nactual:\n%s\n" name expected actual;
    exit 1
  end

let internal_style = { OL.default_style with representation = Internal }
let typed_style = { OL.default_style with representation = FullyTypedInternal }

let () =
  check_repr "surface representation" "surface" "surface";
  check_repr "internal representation" "internal" "internal";
  check_repr "typed representation" "typed" "typed";

  if representation_to_label FullyTypedInternal <> "Fully-Typed Internal" then begin
    Printf.eprintf "OptiLambda representation label test failed\n";
    exit 1
  end;

  check "int literal" (Trm.trm_int 3) "3";

  check "variable" (term "x") "x";

  check_html "surface html with type"
    (Trm.trm_var ~typ:Typ.typ_int (v "x"))
    "<span class=\"optilambda optilambda-surface\" data-optilambda-representation=\"surface\" title=\"type: int\">x</span>";

  check_html "surface html escapes code"
    (Trm.trm_lt ~typ:Typ.typ_bool (term "x") (term "n"))
    "<span class=\"optilambda optilambda-surface\" data-optilambda-representation=\"surface\" title=\"type: bool\">x &lt; n</span>";

  check_html_with_style "internal html representation metadata"
    internal_style
    (Trm.trm_get ~typ:Typ.typ_int (term "p"))
    "<span class=\"optilambda optilambda-internal\" data-optilambda-representation=\"internal\" title=\"type: int\">get(p)</span>";

  check "let" (Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3)) "let x: int = 3";

  check "letmut" (Trm.trm_let (tv "x" (Typ.typ_ptr Typ.typ_int)) (Trm.trm_ref Typ.typ_int (Trm.trm_int 3))) "letmut x = 3";

  check_with_style "internal letmut"
    internal_style
    (Trm.trm_let (tv "x" (Typ.typ_ptr Typ.typ_int)) (Trm.trm_ref Typ.typ_int (Trm.trm_int 3)))
    "let x = ref(3)";

  check_with_style "typed letmut"
    typed_style
    (Trm.trm_let (tv "x" (Typ.typ_ptr Typ.typ_int)) (Trm.trm_ref Typ.typ_int (Trm.trm_int 3)))
    "let x = ref<int>(3)";

  check_with_style "internal uninitialized letmut"
    internal_style
    (Trm.trm_let (tv "x" (Typ.typ_ptr Typ.typ_int)) (Trm.trm_ref_uninit Typ.typ_int))
    "let x = ref_uninit()";

  check_with_style "typed uninitialized letmut"
    typed_style
    (Trm.trm_let (tv "x" (Typ.typ_ptr Typ.typ_int)) (Trm.trm_ref_uninit Typ.typ_int))
    "let x = ref_uninit<int>()";

  check "call" (Trm.trm_apps (term "f") [ term "x"; Trm.trm_int 1 ]) "f(x, 1)";

  check "assignment" (Trm.trm_set (term "x") (Trm.trm_int 4)) "x = 4";

  check_with_style "internal assignment" internal_style (Trm.trm_set (term "x") (Trm.trm_int 4)) "set(x, 4)";

  check_with_style "typed assignment" typed_style (Trm.trm_set (term "x") (Trm.trm_int 4)) "set<int>(x, 4)";

  check_with_style "internal get" internal_style (Trm.trm_get ~typ:Typ.typ_int (term "p")) "get(p)";

  check_with_style "typed get" typed_style (Trm.trm_get ~typ:Typ.typ_int (term "p")) "get<int>(p)";

  check "array access" (Trm.trm_array_get (term "t") (term "i")) "t[i]";

  check_with_style "internal array access" internal_style (Trm.trm_array_access (term "t") (term "i")) "t [+] i";

  check_with_style "typed array access"
    typed_style
    (Trm.trm_array_access ~elem_typ:Typ.typ_int (term "t") (term "i"))
    "Array_Access<int>(t, i)";

  check_with_style "internal array get" internal_style (Trm.trm_array_get (term "t") (term "i")) "get(t [+] i)";

  check_with_style "typed array get"
    typed_style
    (Trm.trm_array_get ~typ:Typ.typ_int (term "t") (term "i"))
    "get<int>(Array_Access<int>(t, i))";

  check "record literal" (Trm.trm_record ~typ:(term "Pair") [ Trm.trm_int 1; Trm.trm_int 2 ]) "{1, 2}";

  check "precedence: add inside mul"
    (Trm.trm_mul ~typ:Typ.typ_int (Trm.trm_add ~typ:Typ.typ_int (term "x") (term "y")) (term "z"))
    "(x + y) * z";

  check "precedence: mul inside add"
    (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_mul ~typ:Typ.typ_int (term "y") (term "z")))
    "x + y * z";

  check "contract call suffix"
    (Trm.trm_apps ~ghost_args:[ (v "h1", Trm.trm_int 3) ] ~ghost_bind:[ (Some (v "z"), v "h2") ] (term "f") [ term "x" ])
    "f(x)[h1 := 3][z : h2]";

  check "function definition"
    (Trm.trm_let_fun (v "f") Typ.typ_int [ tv "x" Typ.typ_int ] (Trm.trm_seq_nomarks [ Trm.trm_abort (Ret (Some (term "x"))) ]))
    "fun f(x) { x }";

  check "function contract"
    (Trm.trm_let_fun ~contract:(FunSpecContract simple_fun_contract) (v "f") Typ.typ_int
       [ tv "x" Typ.typ_int; tv "y" Typ.typ_int ]
       (Trm.trm_seq_nomarks [ Trm.trm_abort (Ret (Some (term "x"))) ]))
    "fun f(x, y) {\n\
    \  requires h_req: x = y;\n\
    \  consumes h_in: R;\n\
    \  ensures h_ens: result = x;\n\
    \  produces h_out: R2;\n\
    \  x\n\
     }";

  check "ghost function"
    (Trm.trm_let_fun ~contract:(FunSpecContract simple_fun_contract) (v "assert_prop")
       (Typ.typ_var (Typ.name_to_typvar "__ghost_ret"))
       [] (Trm.trm_seq_nomarks []))
    "ghost fun assert_prop() {\n  requires h_req: x = y;\n  consumes h_in: R;\n  ensures h_ens: result = x;\n  produces h_out: R2;\n}";

  check "merged consecutive requires"
    (Trm.trm_let_fun ~contract:(FunSpecContract multi_requires_contract) (v "rewrite")
       (Typ.typ_var (Typ.name_to_typvar "__ghost_ret"))
       [] (Trm.trm_seq_nomarks []))
    "ghost fun rewrite() {\n  requires from: int,\n           to: int;\n}";

  check_typ "compact Type result" (Typ.typ_pure_fun [ (v "x", Typ.typ_int) ] Typ.typ_prop) "int -> Prop";

  check_typ "surface C-style pure_fun type"
    (Typ.typ_pure_fun [ (v "n", Typ.typ_int); (v "f", Typ.typ_pure_fun [ (v "i", Typ.typ_int) ] Typ.typ_f32) ] Typ.typ_f32)
    "int * (int -> float) -> float";

  check_typ "surface pure_fun hides __is_true argument type"
    (Typ.typ_pure_fun
       [ (v "n", Typ.typ_int); (v "h", app "__is_true" [ Trm.trm_ge ~typ:Typ.typ_int (term "n") (Trm.trm_int 0) ]) ]
       Typ.typ_prop)
    "int * (n >= 0) -> Prop";

  check "__is_true is hidden in surface"
    (app "__is_true" [ Trm.trm_eq ~typ:Typ.typ_int (term "result") (term "x") ])
    "result = x";

  check "if"
    (Trm.trm_if
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n"))
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_int 1) ])
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_int 0) ]))
    "if (x < n) { x = 1; } else { x = 0; }";

  check "while"
    (Trm.trm_while
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n"))
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "while (x < n) { x = x + 1; }";

  check "for"
    (Trm.trm_for
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in 0..n { x = x + 1; }";

  check "for with step"
    (Trm.trm_for
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 2 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in range(0, n, 2) { x = x + 1; }";

  check "for downward"
    (Trm.trm_for
       { index = v "i"; start = term "n"; direction = DirDown; stop = Trm.trm_int 0; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_sub ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in range(n, 0, -1) { x = x - 1; }";

  check "surface group formula"
    (group_formula "i" (range (Trm.trm_int 0) (term "n") (Trm.trm_int 1)) (app "items" [ term "i" ]))
    "for i in 0..n { items(i) }";

  check "surface group formula with step"
    (group_formula "i" (range (Trm.trm_int 0) (term "n") (Trm.trm_int 2)) (app "items" [ term "i" ]))
    "for i in range(0, n, 2) { items(i) }";

  check "surface points-to formula" (points_to_formula (term "src") (term "H")) "(src ~> H)";

  check_with_style "internal points-to formula" internal_style (points_to_formula (term "src") (term "H")) "(src ~> H)";

  check_with_style "typed points-to formula" typed_style (points_to_formula (term "src") (term "H")) "(src ~> H)";

  check "surface reads contract"
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_reads_contract) (v "read_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun read_example() { reads x: H; }";

  check "surface writes contract"
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_writes_contract) (v "write_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun write_example() { writes x: H; }";

  check "surface preserves contract"
    (Trm.trm_let_fun ~contract:(FunSpecContract preserves_contract) (v "preserve_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun preserve_example() {\n\
    \  preserves ctx: Ctx;\n\
    \  consumes changed: Old;\n\
    \  produces changed_out: New;\n\
     }";

  check "surface local formula printer in contract"
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_formula_contract) (v "formula_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun formula_example() { consumes h: src ~> H; }";

  check "surface generated contract names are hidden"
    (Trm.trm_let_fun ~contract:(FunSpecContract generated_name_cleanup_contract) (v "generated_name_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun generated_name_example() {\n\
    \  consumes Anon,\n\
    \           named: Named,\n\
    \           for #_1 in 0..n -> H(#_1);\n\
     }";

  check "non-adjacent reads and writes recovery"
    (Trm.trm_let_fun ~contract:(FunSpecContract mixed_recovery_contract) (v "mixed_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun mixed_example() {\n\
    \  reads read: ReadH;\n\
    \  writes write: WriteH;\n\
    \  consumes kept: Kept;\n\
    \  produces new_out: Produced;\n\
     }";

  check "alpha-equivalent group reads recovery"
    (Trm.trm_let_fun ~contract:(FunSpecContract alpha_group_reads_contract) (v "alpha_group_read_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun alpha_group_read_example() { reads read: for i in 0..n -> H(i); }";

  check_with_style "internal reads contract"
    internal_style
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_reads_contract) (v "read_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun read_example(): unit [f, x, x] { reads x: H; }";

  check_with_style "typed reads contract"
    typed_style
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_reads_contract) (v "read_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun read_example(): unit [f, x, x] { reads x: H; }";

  check_with_style "internal writes contract"
    internal_style
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_writes_contract) (v "write_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun write_example(): unit [x, x] { writes x: H; }";

  check_with_style "typed writes contract"
    typed_style
    (Trm.trm_let_fun ~contract:(FunSpecContract surface_writes_contract) (v "write_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun write_example(): unit [x, x] { writes x: H; }";

  check_with_style "internal preserves contract"
    internal_style
    (Trm.trm_let_fun ~contract:(FunSpecContract preserves_contract) (v "preserve_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun preserve_example(): unit [ctx, changed, ctx, changed_out] {\n\
    \  preserves ctx: Ctx;\n\
    \  consumes changed: Old;\n\
    \  produces changed_out: New;\n\
     }";

  check_with_style "typed preserves contract"
    typed_style
    (Trm.trm_let_fun ~contract:(FunSpecContract preserves_contract) (v "preserve_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    "fun preserve_example(): unit [ctx, changed, ctx, changed_out] {\n\
    \  preserves ctx: Ctx;\n\
    \  consumes changed: Old;\n\
    \  produces changed_out: New;\n\
     }";

  let surface_focus_expected =
    "fun focus_example() {\n\
    \  requires f: _Fraction;\n\
    \  consumes whole: _RO(f, Whole);\n\
    \  produces wand: Wand(_RO(f, Focused), _RO(f, Whole)),\n\
    \           focused: _RO(f, Focused);\n\
     }"
  in
  let explicit_focus_expected =
    "fun focus_example(): unit [f, whole, wand, focused] {\n\
    \  requires f: _Fraction;\n\
    \  consumes whole: _RO(f, Whole);\n\
    \  produces wand: Wand(_RO(f, Focused), _RO(f, Whole)),\n\
    \           focused: _RO(f, Focused);\n\
     }"
  in
  check "read-only focus contract stays explicit"
    (Trm.trm_let_fun ~contract:(FunSpecContract read_only_focus_contract) (v "focus_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    surface_focus_expected;

  check_with_style "internal read-only focus contract stays explicit"
    internal_style
    (Trm.trm_let_fun ~contract:(FunSpecContract read_only_focus_contract) (v "focus_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    explicit_focus_expected;

  check_with_style "typed read-only focus contract stays explicit"
    typed_style
    (Trm.trm_let_fun ~contract:(FunSpecContract read_only_focus_contract) (v "focus_example") Typ.typ_unit []
       (Trm.trm_seq_nomarks []))
    explicit_focus_expected;

  check "loop contract"
    (Trm.trm_for ~contract:simple_loop_contract
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in 0..n {\n\
    \  requires h_loop: i < n,\n\
    \           h_inv: 0 <= i;\n\
    \  xrequires h_xreq: i < n;\n\
    \  xproduces h_xprod: Done;\n\
    \  x = x + 1;\n\
     }";

  check "compound operator assignment" (Trm.trm_compound_assign ~typ:Typ.typ_int Binop_add (term "r") (Trm.trm_int 2)) "r += 2";

  let mindex =
    app "MINDEX1" [ term "n"; Trm.trm_add ~typ:Typ.typ_int (Trm.trm_mul ~typ:Typ.typ_int (term "bi") (Trm.trm_int 32)) (term "i") ]
  in
  let indexed_product = Trm.trm_mul ~typ:Typ.typ_int (Trm.trm_array_get (term "a") mindex) (Trm.trm_array_get (term "b") mindex) in
  check "compound assignment with indexed product" (Trm.trm_compound_assign ~typ:Typ.typ_int Binop_add (term "s") indexed_product)
    "s += a[MINDEX1(n, bi * 32 + i)] * b[MINDEX1(n, bi * 32 + i)]";

  check "struct access" (Trm.trm_struct_access ~struct_typ:Typ.typ_auto (term "v") "x") "v.x";

  check_with_style "internal struct access" internal_style (Trm.trm_struct_access ~struct_typ:Typ.typ_auto (term "v") "x") "v [.] x";

  check_with_style "internal struct get"
    internal_style
    (Trm.trm_struct_get ~field_typ:Typ.typ_int ~struct_typ:(Typ.typ_var (Typ.name_to_typvar "Pair")) (term "v") "x")
    "get(v [.] x)";

  check_with_style "typed struct access"
    typed_style
    (Trm.trm_struct_access ~field_typ:Typ.typ_int ~struct_typ:(Typ.typ_var (Typ.name_to_typvar "Pair")) (term "v") "x")
    "Record_Access<int>(v, x)";

  check_with_style "typed struct get"
    typed_style
    (Trm.trm_struct_get ~field_typ:Typ.typ_int ~struct_typ:(Typ.typ_var (Typ.name_to_typvar "Pair")) (term "v") "x")
    "get<int>(Record_Access<int>(v, x))";

  check_with_style "internal resource formula" internal_style (Trm.trm_apps (term "cell") [ typed_term "v" Typ.typ_int ]) "cell(v)";

  check_with_style "typed resource formula" typed_style (Trm.trm_apps (term "cell") [ typed_term "v" Typ.typ_int ]) "cell<int>(v)";

  check "ghost call" ghost_call_example "ghost(rewrite, \"h := x = y\", \"z <- h_out\")";

  check "surface ghost call uses C-style arguments"
    arbitrary_pure_fun_ghost
    "ghost(assert_inhabited, \"x := arbitrary(int * (int -> float) -> float)\", \"reduce_sum <- x\")";

  check "surface hides __ghost_fn type"
    (Trm.trm_let (tv "focusA" (Typ.typ_var (Typ.name_to_typvar "__ghost_fn"))) (term "body"))
    "let focusA = body";

  check_with_style "style hides types" { OL.default_style with print_types = false }
    (Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3))
    "let x = 3";

  check_with_style "style omits default loop step"
    { OL.default_style with omit_default_loop_step = true }
    (Trm.trm_for
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in 0..n { x = x + 1; }";

  check_with_style "style hides contracts"
    { OL.default_style with print_contracts = false }
    (Trm.trm_let_fun ~contract:(FunSpecContract simple_fun_contract) (v "f") Typ.typ_int
       [ tv "x" Typ.typ_int; tv "y" Typ.typ_int ]
       (Trm.trm_seq_nomarks [ Trm.trm_abort (Ret (Some (term "x"))) ]))
    "fun f(x, y) { x }";

  check "marks" (Mark.trm_add_mark "target" (term "x")) "@marks[target] x"
