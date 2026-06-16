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
    "fun f(x: int): int { x }";

  check "function contract"
    (Trm.trm_let_fun ~contract:(FunSpecContract simple_fun_contract) (v "f") Typ.typ_int
       [ tv "x" Typ.typ_int; tv "y" Typ.typ_int ]
       (Trm.trm_seq_nomarks [ Trm.trm_abort (Ret (Some (term "x"))) ]))
    "fun f(x: int, y: int): int [h_req, h_in, h_ens, h_out] {\n\
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
    "ghost fun rewrite() {\n  requires from: int,\n           to: int,\n           inside: pure_fun(fun(x: int): Prop);\n}";

  check_typ "compact Type result" (Typ.typ_pure_fun [ (v "x", Typ.typ_int) ] Typ.typ_prop) "pure_fun(fun(x: int): Prop)";

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
    "for<seq> i in 0..n:2 { x = x + 1; }";

  check "for downward"
    (Trm.trm_for
       { index = v "i"; start = term "n"; direction = DirDown; stop = Trm.trm_int 0; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_sub ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in n..0:-1 { x = x - 1; }";

  check "loop contract"
    (Trm.trm_for ~contract:simple_loop_contract
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [ Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1)) ]))
    "for<seq> i in 0..n [h_loop, h_inv, h_xreq, h_xprod] {\n\
    \  requires h_loop: i < n,\n\
    \           h_inv: 0 <= i;\n\
    \  xrequires h_xreq: i < n;\n\
    \  xproduces h_xprod: Done;\n\
    \  x = x + 1;\n\
     }";

  check "compound operator call" (Trm.trm_compound_assign ~typ:Typ.typ_int Binop_add (term "r") (Trm.trm_int 2)) "(+=)(r, 2)";

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

  check "ghost call" ghost_call_example "ghost(rewrite()[h := x = y][z : h_out])";

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
    "fun f(x: int, y: int): int { x }";

  check "marks" (Mark.trm_add_mark "target" (term "x")) "@marks[target] x"
