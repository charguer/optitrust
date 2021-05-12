open Clang.Ast
open Optitrust__Ast
open Optitrust__Paths
open Path_constructors
open Optitrust__Clang_ast_parser
open Optitrust__Translate_ast

(*
  tests to be executed:
  list of (filename, list of (path, expected outputs))

  note: the type contains target instead of path because lists of smart
  constructors build targets. A use of List.flatten in test_file gives back a
  path.
 *)
let tests : (string * ((target * (expl_target)) list)) list =
  [
    ("test_swap_coordinates/test_swap_coordinates.cpp",
     [
       ([[cVarDef "t"] >> [cFor "j"]],
        [
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0]
        ]
       );
       ([cFunDef "main";
         cCall ~args:[cVar "t"] ~validate:(List.mem true) ()],
        [
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0; Dir_nth 0; Dir_body; Dir_nth 0; Dir_arg 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0; Dir_nth 0; Dir_body; Dir_nth 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0; Dir_nth 0; Dir_body; Dir_nth 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0;
           Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 1];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 1];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 1];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1]
        ]
       );
       ([cFunDef "main" ;
         cCall ~args:[cVar target ~name:"t" ()] ~validate:(List.mem true)
           ()],
        [
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0; Dir_nth 0; Dir_body; Dir_nth 0; Dir_arg 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0;
           Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 1];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 1];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 1]
        ]
       );
       ([cFunDef "main";
         cStr ~regexp:true "t\\[.\\]\\[.\\]"],
        [
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0; Dir_nth 0; Dir_body; Dir_nth 0; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0]
        ]
       );
       ([cStr ~regexp:true "t\\[.\\]\\[.\\]"; cInt 2],
        [
          [Dir_nth 0; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_arg 0;
           Dir_arg 1];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0;
           Dir_arg 1]
        ]
       );
       ([cFunDef "main";
         cInstrSubstr ~regexp:true "t\\[.\\]\\[.\\]"],
        [
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 2; Dir_nth 0; Dir_body;
           Dir_nth 0; Dir_nth 0; Dir_body; Dir_nth 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body]
        ]
       );
       ([cInstrSubstr "ret"],
        [
          [Dir_nth 0; Dir_nth 1; Dir_body; Dir_nth 0];
          [Dir_nth 0; Dir_nth 2; Dir_body; Dir_nth 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 4]
        ]
       );
       ([cAccesses ~accesses:[cAccess; cIndex ~index:[cStr "2"] ()] ()],
        [
          [Dir_nth 0; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_arg 0];
          [Dir_nth 0; Dir_nth 3; Dir_body; Dir_nth 3; Dir_body; Dir_nth 1;
           Dir_arg 1; Dir_arg 0; Dir_arg 0; Dir_arg 0; Dir_arg 0]
        ]
       )
     ]
    );
    ("testPIC/picGoal.cpp",
     [
       ([cVarDef "i"],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 48]
        ]
       );
       ([cSwitch ()],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 57]
        ]
       );
       ([cSwitch ~cond:[cVar ~name:"sim_distrib" ()] ()],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 57]
        ]
       );
       ([cFor
           ~init:[cVar ~name:"i" ()]
           ~body:[cSet "my_chunk"]
           ()],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 94; Dir_body;
           Dir_nth 1; Dir_body; Dir_nth 0];
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 9; Dir_nth 1; Dir_body; Dir_nth 0; Dir_body; Dir_nth 0;
           Dir_body; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_nth 2;
           Dir_body; Dir_nth 0]
        ]
       );
       ([cSet ~lhs:[cVar ~name:"diag_energy" ()] ()],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 3];
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 4];
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 5];
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 6];
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 7]
        ]
       );
       ([cFor ~init:[cSet ~lhs:[cVar ~name:"i_color" ()]] "";
         cFor "my_chunk"
        ],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 9; Dir_nth 1; Dir_body; Dir_nth 0; Dir_body; Dir_nth 0;
           Dir_body; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_nth 2]
        ]
       );
       (* path which matches several times the same node *)
       ([cFor "i_color"];
         cFor "my_chunk"]
        ],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 9; Dir_nth 1; Dir_body; Dir_nth 0; Dir_body; Dir_nth 0;
           Dir_body; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_nth 2]
        ]
       );
       ([cCall ~fun_:[cVar ~name:"bag_push" ~exact:false ()] ()],
        [
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 9; Dir_nth 1; Dir_body; Dir_nth 0; Dir_body; Dir_nth 0;
           Dir_body; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_nth 2;
           Dir_body; Dir_nth 1; Dir_body; Dir_nth 5; Dir_then; Dir_nth 1];
          [Dir_nth 0; Dir_nth 15; Dir_nth 11; Dir_body; Dir_nth 95; Dir_body;
           Dir_nth 9; Dir_nth 1; Dir_body; Dir_nth 0; Dir_body; Dir_nth 0;
           Dir_body; Dir_nth 2; Dir_body; Dir_nth 0; Dir_body; Dir_nth 2;
           Dir_body; Dir_nth 1; Dir_body; Dir_nth 5; Dir_else; Dir_nth 1]
        ]
       )
     ]
    )
  ]

let rec compare_explicit_paths (epl : expl_target)
  (epl' : expl_target) : unit =
  match epl, epl' with
  | [], [] -> print_info None "The result is correct.\n"
  | ep :: epl, ep' :: epl' ->
     begin
       try
         if List.exists2 (fun d d' -> d <> d') ep ep' then
           fail None "The result does not match the expected outcome";
         compare_explicit_paths epl epl'
       with
       | Invalid_argument _ ->
          fail None "The result does not match the expected outcome"
     end
  | _ -> fail None "The result does not match the expected outcome"

let test_path (ast : trm) (p : path) (expected_output : expl_target) : unit =
  print_info None "Resolving path...\n";
  let flag = !Optitrust__Flags.verbose in
  Optitrust__Flags.verbose := false;
  let epl = resolve_path p ast in
  Optitrust__Flags.verbose := flag;
  print_info None "Path resolution done. Result:\n";
  begin match epl with
  | [] -> print_info None "No subterm matched the path\n"
  | _ ->
     let sl = List.map path_to_string epl in
     let tl =
       List.map (fun dl -> let (t, _) = resolve_path dl ast in t) epl
     in
     let stl = List.combine sl tl in
     List.iteri
       (fun i (s, t) ->
         print_info None "Explicit path %d: %s\n" i s;
         match t.loc with
         | None ->
            print_info None "Corresponding term:\n%s\n" (ast_to_string t)
         | Some (_, line) ->
            print_info None "Corresponding term at line %d:\n%s\n"
              line (ast_to_string t)
       )
       stl
  end;
  print_info None "Checking result...\n";
  compare_explicit_paths epl expected_output

let test_file (filename : string)
  (pl_epl_l : (target * (expl_target)) list) : unit =
  print_info None "Parsing file...\n";
  let clang_ast = parse_file filename in
  print_info None "Parsing Done.\n";
  (* print_info None "AST:\n%s\n"
   *   (Clangml_show.show_translation_unit clang_ast); *)
  print_info None "Translating AST...\n";
  let ast = translate_ast clang_ast in
  print_info None "Translation done.\n";
  (* print_info None "AST:\n";
   * if !Flags.verbose then
   *   begin print_ast stdout ast; print_newline () end; *)
  List.iteri
    (fun i (pl, epl) ->
      let p = List.flatten pl in
      print_info None "\nPath %d: %s\n" i (path_to_string p);
      test_path ast p epl
    )
    pl_epl_l

let _ =
  Arg.parse
    Optitrust__Flags.spec
    (fun _ -> raise (Arg.Bad "Error: no argument expected"))
    ("usage: no argument expected, only options");
  List.iteri
    (fun i (filename, pl_epl_l) ->
      print_info None "\nFile %d: %s\n" i filename;
      test_file filename pl_epl_l
    )
    tests
