A transformation script must have the following form:

```ocaml
open Script_tools

let _ =
  run
    (fun () ->
       (* ... here include script instructions ... *)
    )
```

Script instructions form a sequence of OCaml commands that use and combine basic
instructions/transformations whose current list is the following one:

- `set_init_source filename`: this is the mandatory first instruction. Set the
  environment for a script execution on the given file. In particular, set a
  default prefix for output file names as the current file name.

- `reset ()`: reset the environment to allow for a new call to `set_init_source`
  without dealing with the full transformation history of the previous file.

- `dump ~out_prefix:name ()`: depending on the set options, output either the
  current source code/AST or the successive states of the source code/AST up to
  the current one, with the following naming convention:
  + The initial source code/AST is in `name_input.cpp`/`name_init.ast`.
  + The current source code/AST is in `name_output.cpp`/`name_output.ast`.
  + The intermediate source codes/AST are numbered as
  `name_index.cpp`/`name_index.ast`.

- `switch ~only_branch:i fl`: branch according to the case list. Each case is a
  function of type `unit -> unit`, where the body of the function is a sequence
  of script instructions. Execute all branches by default, execute only branch
  `i` if optional argument is provided.

- `add_label l p`: label the C/C++ instruction pointed by the path `p` with
  `l`. If the path points to several instructions, they are numbered with
  `l_index`.

- `delete_label l`: delete the label `l` from the current program.

- `delete_labels sl` : delete each of the labels which have a prefix in `sl`.

- `swap_coordinates ~name type_var`: swap the two first dimensions of a
  multidimensional array type. To avoid swapping all occurrences of such a type,
  use a type variable to choose which occurrences to swap. Functions applied to
  these occurrences will be copied and named through `name`.

- `split_sequence ~keep_labels ~labels:sl ~split_name:name_func p`: split the
  sequence around the instruction pointed by `p`. Copies of variables may be
  introduced, their name is chosen through the `name_func` optional argument (by
   default: `x` becomes `x_split`). Labels are computed from the list `sl`:
    + `sl = [] -> (result, result_block1, result_block2)`
    + `sl = [s] -> (s, s_block1, s_block2)`
    + `sl = [l1, l2, l3] -> (l1, l2, l3)`
  These labels annotate the result sequence and each block. The `keep_labels`
  boolean, false by default, tells whether these labels must be kept.

- `extract_loop_var ~keep_label ~label:l p`: extract the declaration of a
  variable from a for loop as the declaration of an array. The loop body must
  start with the declaration and the variable should be heap allocated without
  initialisation. The path `p` must lead to the loop. Label the result with `l`
  (if `l = ""`, then the label is `result`). The `keep_label` boolean, false by
  default, tells whether this label must be kept. The loop might be labelled and
  its label will be kept.

- `extract_loop_vars ~keep_label ~label p`: same as extract_loop_var but extract
  as many variables as possible.

- `split_loop_nodep ~keep_labels ~labels:sl p`: loop splitting for loops whose
  body is made of two independent blocks. The path `p` must lead to the loop.
  Labels are computed from the list `sl`:
    + `sl = [] -> (result, result_loop1, result_loop2)`
    + `sl = [s] -> (s, s_loop1, s_loop2)`
    + `sl = [l1, l2, l3] -> (l1, l2, l3)`
  These labels annotate the result sequence and each loop. The `keep_labels`
  boolean, false by default, tells whether these labels must be kept. The loop
  might be labelled and its label will be deleted (the `result_loop` labels
  replace it).

- `split_loop ~keep_labels ~labels ~split_name p`: loop splitting. Label
  management is the same as for `split_loop_nodep`. See the documentation of
  `split_sequence` for the argument `split_name`.

- `tile_array ~name ~block_name ~block_size type_var`: tile all arrays whose
  type is provided, using the given block_size. `block_name` is used to insert a
  declaration for the type of the blocks. `name` has the same purpose as in
  `swap_coordinates`.

- `fold_decl ~as_reference ~fold_at ~decl_path ()`: fold the declaration pointed
  at by the given path. If the list of paths `fold_at` is provided, fold only in
  the subterms pointed at by these paths. If the declaration is of the form
  `x = &dx` and `as_reference` is set to true (false by default), then replace
  `dx` with `*x` instead of `&dx` with `x`. This can also be used for type
  declarations.

- `insert_decl ~insert_before ~insert_after ~const ~as_reference ~name ~value ()`:
  insert a definition `name = value` either before the subterm pointed at by
  `insert_before` or after the one pointed at by `insert_after` (use only
  one). The declaration may be `const` qualified (not by default) and a
  reference (`name = &value`, not by default).

- `insert_const ~insert_before ~insert_after ~name ~value ()`: same as
  `insert_decl` but only for constant declarations.

- `insert_and_fold ~insert_before ~insert_after ~const ~as_reference ~fold_at ~name ~value ()`:
  combine `insert_decl` and `fold_decl`.

- `insert_typedef ~insert_before ~insert_after ~name ~value ()`: same as
  `insert_decl` but for type definitions.

- `insert_and_fold_typedef ~insert_before ~insert_after ~fold_at ~name ~value ()`:
   same as `insert_and_fold` but for type definitions.

- `remove_decl ~decl_path ()`: remove the declaration pointed at by the given
  path. The declared object should not be used in the program.

- `inline_decl ~delete_decl ~inline_at ~fun_result ~fun_return_label ~decl_path ()`:
  inline the (variable/type/function) declaration pointed at by the given
  path. If the list of paths `inline_at` is provided, inline only in the
  subterms pointed at by these paths. If `delete_decl` is set to true (false by
  default), also remove the declaration. In case of function inlining, it is
  possible to give a name to its result through `fun_result` (res by default)
  and to choose the label used to insert `goto` statements corresponding to
  `return` statements through `fun_return_label` (exit by default).
- `tile_loop p`: tile the loop pointed at by `p` if it has the appropriate
  shape, i.e.
  ```
  optional_label:
  for (i = 0; i < N; i++) {
    int i1 = i / block_size;
    int i2 = i % block_size;
    body
  }
  ```
- `aos_to_soa ~name type_var`: apply the Array of Structures to Structure of
  Arrays transformation to the arrays whose type is provided. `name` has the
  same purpose as in `swap_coordinates`.

- `eliminate_goto_next ()`: globally eliminate all `goto` statements whose label
  is around the next instruction.

- `group_decl_init ()`: globally replace all variable declarations without
  initialisation followed by an initialisation statement with the corresponding
  declaration with initialisation.

- `inline_seq ~seq_path ()`: inline the seq pointed at by the given path into
  its container.

- `add_attributes s p`: add the attribute described by string `s` to the term
  pointed at by path `p`.

Throughout a transformation script, a stack of the successive states of the
program is maintained, each transformation adding a new program on top of this
stack. All transformations given above have an optional argument `replace_top`
which, when set to true (false by default), means that they replace the program
at the top instead of adding a new one.

See [`hello_world.ml`](test_suite/hello_world/hello_world.ml) for an example
script and the accompanying [`Makefile`](test_suite/hello_world/Makefile) to
know how to execute it once this library is installed.

See [`PATH.md`](PATH.md) for instructions on how to use paths.