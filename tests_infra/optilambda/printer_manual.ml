open Optitrust_ast
open Ast

[@@@warning "-42"]

module OL = Optitrust_optilambda.Optilambda
open Optitrust_optilambda.Optilambda_style

let v name =
  Ast.name_to_var name

let tv name ty =
  v name, ty

let term name =
  Trm.trm_var (v name)

let print_example title trm =
  Printf.printf "-- %s --\n%s\n\n" title (OL.trm_to_string trm)

let print_example_with_style title style trm =
  Printf.printf "-- %s --\n%s\n\n" title (OL.trm_to_string ~style trm)

let print_type_example title ty =
  Printf.printf "-- type: %s --\n%s\n\n" title (OL.typ_to_string ty)

let incr_x =
  Trm.trm_set (term "x") (Trm.trm_add ~typ:Typ.typ_int (term "x") (Trm.trm_int 1))

let resource_set ?(pure = []) ?(linear = []) () =
  { empty_resource_set with pure; linear }

let fun_contract_example =
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

let loop_contract_example =
  {
    empty_loop_contract with
    loop_ghosts = [v "h_loop", Trm.trm_lt ~typ:Typ.typ_int (term "i") (term "n")];
    invariant = resource_set
      ~pure:[v "h_inv", Trm.trm_le ~typ:Typ.typ_int (Trm.trm_int 0) (term "i")]
      ~linear:[v "h_pres", term "Acc"]
      ();
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

let ghost_begin_example =
  let inner_ghost =
    Trm.trm_ghost_force
      (Trm.ghost_call
         ~ghost_bind:[Some (v "rev"), "reverse"]
         (v "focus")
         ["h", term "R"])
  in
  Trm.trm_add_attribute GhostInstr
    (Trm.trm_let
       (v "scope", Typ.typ_auto)
       (Trm.trm_apps (term "__ghost_begin") [inner_ghost]))

let ghost_end_example =
  Trm.trm_add_attribute GhostInstr
    (Trm.trm_apps (term "__ghost_end") [term "scope"])

let loop (mode : loop_mode) (direction : loop_dir) =
  Trm.trm_for
    ~mode
    { index = v "i"; start = Trm.trm_int 0; direction; stop = term "n"; step = Trm.trm_int 1 }
    (Trm.trm_seq_nomarks [incr_x])

let alias_typedef =
  Trm.trm_typedef {
    typedef_name = v "Index";
    typedef_body = Typedef_alias Typ.typ_int;
  }

let record_typedef =
  Trm.trm_typedef {
    typedef_name = v "Pair";
    typedef_body = Typedef_record [
      Record_field ("fst", Typ.typ_int), Access_public;
      Record_field ("snd", Typ.typ_int), Access_public;
    ];
  }

let enum_typedef =
  Trm.trm_typedef {
    typedef_name = v "Color";
    typedef_body = Typedef_enum [
      v "Red", Some (Trm.trm_int 0);
      v "Blue", None;
    ];
  }

let () =
  let x = v "x" in

  print_type_example "int"
    Typ.typ_int;

  print_type_example "ptr"
    (Typ.typ_ptr Typ.typ_int);

  print_type_example "array"
    (Typ.typ_array ~size:(term "n") Typ.typ_int);

  print_type_example "function type"
    (Typ.typ_fun [Typ.typ_int; Typ.typ_bool] Typ.typ_int);

  print_type_example "const type"
    (Typ.typ_const Typ.typ_int);

  print_type_example "array without size"
    (Typ.typ_array Typ.typ_int);

  print_type_example "arbitrary type"
    (Typ.typ_arbitrary "custom_t");

  print_type_example "unknown type constructor"
    (Typ.typ_apps (term "Vector") [Typ.typ_int; term "n"]);

  print_example "literal" (Trm.trm_int 3);

  print_example "unit literal" (Trm.trm_unit ());

  print_example "boolean literal" (Trm.trm_bool true);

  print_example "float literal" (Trm.trm_float 3.5);

  print_example "string literal" (Trm.trm_string "hello");

  print_example "null literal" (Trm.trm_null Typ.typ_int);

  print_example "variable" (term "x");

  print_example "marked variable"
    (Mark.trm_add_mark "target" (term "x"));

  print_example "direct primitive"
    (Trm.trm_prim Typ.typ_int (Prim_binop Binop_add));

  print_example "let"
    (Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3));

  print_example "letmut"
    (Trm.trm_let_mut (tv "x" Typ.typ_int) (Trm.trm_int 3));

  print_example "letmut uninit"
    (Trm.trm_let_mut_uninit (tv "x" Typ.typ_int));

  print_example "let multiple"
    (Trm.trm_let_mult [
      tv "x" Typ.typ_int, Trm.trm_int 1;
      tv "y" Typ.typ_int, Trm.trm_int 2;
    ]);

  print_example "predeclaration"
    (Trm.trm_predecl (tv "x" Typ.typ_int));

  print_example "sequence with return"
    (Trm.trm_seq_nomarks ~result:x [
      Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3);
      incr_x;
    ]);

  print_example "anonymous function"
    (Trm.trm_fun [tv "x" Typ.typ_int] Typ.typ_int
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]));

  print_example "function definition"
    (Trm.trm_let_fun
       (v "f")
       Typ.typ_int
       [tv "x" Typ.typ_int; tv "y" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]));

  print_example "generic function definition"
    (Trm.trm_template
       [v "A", Typename None]
       (Trm.trm_let_fun
          (v "id")
          (term "A")
          [tv "x" (term "A")]
          (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))])));

  print_example "function contract"
    (Trm.trm_let_fun
       ~contract:(FunSpecContract fun_contract_example)
       (v "contracted")
       Typ.typ_int
       [tv "x" Typ.typ_int; tv "y" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]));

  print_example "function reverts contract"
    (Trm.trm_let_fun
       ~contract:(FunSpecReverts (v "forward"))
       (v "backward")
       Typ.typ_unit
       []
       (Trm.trm_seq_nomarks []));

  print_example "call"
    (Trm.trm_apps (term "f") [term "x"; Trm.trm_int 1]);

  print_example "call with contract arguments"
    (Trm.trm_apps
       ~ghost_args:[v "h1", Trm.trm_int 3; v "h2", Trm.trm_eq ~typ:Typ.typ_int (term "x") (term "y")]
       ~ghost_bind:[Some (v "z"), v "h2"]
       (term "f")
       [term "x"; term "y"]);

  print_example "assignment"
    (Trm.trm_set (term "x") (Trm.trm_int 4));

  print_example "operator precedence"
    (Trm.trm_mul ~typ:Typ.typ_int
       (Trm.trm_add ~typ:Typ.typ_int (term "x") (term "y"))
       (Trm.trm_sub ~typ:Typ.typ_int (term "z") (Trm.trm_int 1)));

  print_example "comparisons and bit operators"
    (Trm.trm_seq_nomarks [
      Trm.trm_eq ~typ:Typ.typ_int (term "x") (term "y");
      Trm.trm_neq ~typ:Typ.typ_int (term "x") (term "y");
      Trm.trm_le ~typ:Typ.typ_int (term "x") (term "n");
      Trm.trm_ge ~typ:Typ.typ_int (term "x") (Trm.trm_int 0);
      Trm.trm_apps (Trm.trm_binop Typ.typ_int Binop_bitwise_and) [term "x"; term "mask"];
      Trm.trm_apps (Trm.trm_binop Typ.typ_int Binop_bitwise_or) [term "x"; term "mask"];
      Trm.trm_apps (Trm.trm_binop Typ.typ_int Binop_xor) [term "x"; term "mask"];
      Trm.trm_apps (Trm.trm_binop Typ.typ_int Binop_shiftl) [term "x"; Trm.trm_int 2];
      Trm.trm_apps (Trm.trm_binop Typ.typ_int Binop_shiftr) [term "x"; Trm.trm_int 2];
    ]);

  print_example "unary operators"
    (Trm.trm_seq_nomarks [
      Trm.trm_get (term "p");
      Trm.trm_address_of (term "x");
      Trm.trm_minus ~typ:Typ.typ_int (term "x");
      Trm.trm_neg (term "ok");
      Trm.trm_apps (Trm.trm_unop Typ.typ_int (Unop_cast Typ.typ_int)) [term "x"];
      Trm.trm_apps (Trm.trm_unop Typ.typ_int Unop_bitwise_neg) [term "x"];
      Trm.trm_apps (Trm.trm_unop Typ.typ_int Unop_post_incr) [term "x"];
    ]);

  print_example "compound assignment"
    (Trm.trm_compound_assign ~typ:Typ.typ_int Binop_add (term "x") (Trm.trm_int 1));

  print_example "allocations and aggregate literals"
    (Trm.trm_seq_nomarks [
      Trm.trm_new Typ.typ_int (Trm.trm_int 3);
      Trm.trm_new_uninit Typ.typ_int;
      Trm.trm_delete (term "p");
      Trm.trm_array ~elem_typ:Typ.typ_int [Trm.trm_int 1; Trm.trm_int 2];
      Trm.trm_record ~typ:(term "Pair") [term "x"; term "y"];
    ]);

  print_example "array access"
    (Trm.trm_array_get (term "t") (term "i"));

  print_example "nested array access"
    (Trm.trm_array_get (Trm.trm_array_get (term "t") (term "i")) (term "j"));

  print_example "struct access"
    (Trm.trm_seq_nomarks [
      Trm.trm_struct_get ~struct_typ:(term "Pair") (term "s") "fst";
      Trm.trm_apps (Trm.trm_unop Typ.typ_auto (Unop_struct_access "fst")) [term "p"];
    ]);

  print_example "if"
    (Trm.trm_if
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n"))
       (Trm.trm_seq_nomarks [Trm.trm_set (term "x") (Trm.trm_int 1)])
       (Trm.trm_seq_nomarks [Trm.trm_set (term "x") (Trm.trm_int 0)]));

  print_example "while"
    (Trm.trm_while
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n"))
       (Trm.trm_seq_nomarks [incr_x]));

  print_example "do while"
    (Trm.trm_do_while
       (Trm.trm_seq_nomarks [incr_x])
       (Trm.trm_lt ~typ:Typ.typ_int (term "x") (term "n")));

  print_example "for sequential up"
    (loop Sequential DirUp);

  print_example "for parallel up_eq"
    (loop (Parallel : loop_mode) DirUpEq);

  print_example "for with contract"
    (Trm.trm_for
       ~contract:loop_contract_example
       { index = v "i"; start = Trm.trm_int 0; direction = DirUp; stop = term "n"; step = Trm.trm_int 1 }
       (Trm.trm_seq_nomarks [incr_x]));

  print_example "strict for with contract"
    (Trm.trm_for
       ~contract:{ loop_contract_example with strict = true; parallel_reads = [v "h_read", term "RO"] }
       { index = v "i"; start = Trm.trm_int 0; direction = DirUpEq; stop = term "n"; step = Trm.trm_int 2 }
       (Trm.trm_seq_nomarks [incr_x]));

  print_example "for gpu_thread down"
    (loop GpuThread DirDown);

  print_example "for magic_thread down_eq"
    (loop MagicThread DirDownEq);

  print_example "for_c"
    (Trm.trm_for_c
       (Trm.trm_let (tv "i" Typ.typ_int) (Trm.trm_int 0))
       (Trm.trm_lt ~typ:Typ.typ_int (term "i") (term "n"))
       (Trm.trm_set (term "i") (Trm.trm_add ~typ:Typ.typ_int (term "i") (Trm.trm_int 1)))
       (Trm.trm_seq_nomarks [incr_x]));

  print_example "switch"
    (Trm.trm_switch
       (term "tag")
       [
         [Trm.trm_int 0], Trm.trm_seq_nomarks [Trm.trm_set (term "x") (Trm.trm_int 0)];
         [Trm.trm_int 1; Trm.trm_int 2], Trm.trm_seq_nomarks [Trm.trm_set (term "x") (Trm.trm_int 1)];
         [], Trm.trm_seq_nomarks [Trm.trm_abort (Break None)];
       ]);

  print_example "typedef alias"
    alias_typedef;

  print_example "typedef record"
    record_typedef;

  print_example "typedef enum"
    enum_typedef;

  print_example "namespace"
    (Trm.trm_namespace "M" (Trm.trm_seq_nomarks [Trm.trm_let (tv "x" Typ.typ_int) (Trm.trm_int 3)]) false);

  print_example "extern"
    (Trm.trm_extern "C" [Trm.trm_let_fun (v "f") Typ.typ_int [tv "x" Typ.typ_int] (Trm.trm_seq_nomarks [])]);

  print_example "template fallback"
    (Trm.trm_template
       [v "A", Typename None; v "N", NonType (Typ.typ_int, Some (Trm.trm_int 4))]
       (Trm.trm_let (tv "x" (term "A")) (term "value")));

  print_example "using directive"
    (Trm.trm_using_directive "std");

  print_example "abort forms"
    (Trm.trm_seq_nomarks [
      Trm.trm_abort (Ret None);
      Trm.trm_abort (Ret (Some (term "x")));
      Trm.trm_abort (Break None);
      Trm.trm_abort (Break (Some "exit_label"));
      Trm.trm_abort (Continue None);
      Trm.trm_abort (Continue (Some "next_label"));
    ]);

  print_example "goto"
    (Trm.trm_goto "exit_label");

  print_example "arbitrary code"
    (Trm.code (Expr "opaque_expr()"));

  print_example "omp routine"
    (Trm.trm_omp_routine Get_thread_num);

  print_example "ghost call"
    ghost_call_example;

  print_example "ghost begin"
    ghost_begin_example;

  print_example "ghost end"
    ghost_end_example;

  print_example_with_style "style: no types"
    { OL.default_style with print_types = false }
    (Trm.trm_let_fun
       (v "f")
       Typ.typ_int
       [tv "x" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]));

  print_example_with_style "style: no contracts"
    { OL.default_style with print_contracts = false }
    (Trm.trm_let_fun
       ~contract:(FunSpecContract fun_contract_example)
       (v "contracted")
       Typ.typ_int
       [tv "x" Typ.typ_int; tv "y" Typ.typ_int]
       (Trm.trm_seq_nomarks [Trm.trm_abort (Ret (Some (term "x")))]));

  print_example_with_style "style: no ghosts"
    { OL.default_style with print_ghosts = false }
    ghost_call_example;

  print_example_with_style "style: no marks"
    { OL.default_style with print_marks = false }
    (Mark.trm_add_mark "target" (term "x"));

  print_example_with_style "style: no loop direction and no default step"
    { OL.default_style with print_loop_direction = false; omit_default_loop_step = true }
    (loop Sequential DirUp);

  print_example_with_style "style: full loop mode names"
    { OL.default_style with loop_mode_style = Full }
    (loop GpuThread DirUp)
