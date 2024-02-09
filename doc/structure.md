
# Overall architecture

- `ast.ml`

A program is represented by its abstract syntax tree (AST). It corresponds to an imperative lambda-calculus, in which there is no notion of left-value nor of mutable variables. (These C features are encoded, as described further on.)

- The type `trm` describes such an ast.
  + The `desc` field of type `trm_desc` indicates the type of the term, for example `Trm_if` describes a conditional.
  + The `annot` field stores meta-data, including C display styles (e.g., `*p.f` vs `p->f`).
  + The `loc` field stores the location in the source file.
  + The `typ` and `ctx` fields store typing information.
  + The `is_statement` caches the information of whether a term corresponds to a statement or an expression.

- The grammar of terms includes values (type `value`), literals (type `lit`), primitive operators (type `prim`, and `unop` and `binop`).

- ASTs of type `trm` are built using "smart constructors", e.g. `trm_for`, which takes as argument the range and the body.

- To inspect a term, pattern matching is technically possible yet discouraged, one should prefer using "smart invertors", e.g. `trm_for_inv`, which takes a term, and returns an option with the range and the body.

- The file also contains a few test functions ("predicates") such as `is_infix_prim_fun`.

# C front-end: parsing, encoding and decoding

1. The C code is parsed using the Clang framework.

2. The Clang AST is obtained as an OCaml data structure using the OCaml clangml package.

3. The clangml AST
 is converted into "raw OptiTrust AST" by file `clang_to_astRawC`. At this stage, `x = 4` is represented (roughly) as `Trm_app(Prim_op_set, [Trm_var "x"; Trm_lit (Lit_int 4)])`.

4. The "raw AST" is encoded into the OptiTrust AST by eliminating mutable variables and left-values. For example, `int x = 4` becomes `int* x = new int(4)`. This encoding is implemented by the function `cfeatures_elim` in file `Ast_fromto_AstC.ml`.
   [FIXME: This currently does not respect *linearity* of targets. We want that each targettable trm node in the original AST appears exactly once (no duplication, no suppression) in the encoded AST. This is for example not the case for array and structure access in the current code.
   One allowed transformation is to encode C sequences as blocks of let, terminated by a return value]

5. Reciprocally, the decoding phase implemented by the function `cfeatures_intro` in the same file `Ast_fromto_AstC.ml` converts OptiTrust AST into "raw AST". Annotations (stored in field `annot` of type `trm`) are exploited to guide the output style.

6. The file `astC_to_c` prints the "raw AST" into C syntax. The code is optionally beautified using `clangformat`.

# Transformations

A transformation is an operation that updates the AST at hand.

Most transformations operate on a subterm (i.e. a subtree of the full AST).

To locate the subterm to be transformed, OptiTrust leverages the notions of "paths" and "target".

# Paths

A "path" in an AST is represented as a list of directions (type `dir` in `path.ml`, e.g. `Dir_then` follows a then branch from a conditional node in the AST).

- The function `apply_on_path (transfo : trm -> trm) (t : trm) (p : path) : trm` takes a full AST described by `t`, takes a path (i.e. list of directions) described by `p`, and replaces the subterm of `t` at that path `p` with the result of applying the transformation `transfo` to that subterm.

- The function `resolve_path_and_ctx (p : path) (t : trm) : trm * (trm list)` takes a full AST described by `t`, a path described by `p`, and it returns (1) the subterm of `t` reached by that path `p`, and (2) the list of the nodes found on the path between the root and the endpoint of the path described by `p`. [TODO: specify the order of the list]

# Target

The "target" system provides a concise means of describing paths.

For example `[cFunDef "foo"; cFor "i"; cFor "j"]` finds a for-loop with index named "j", nested inside a for-loop with index named "i", inside the body of the function named "foo".

A target consists of a list of constraints, e.g. `cFor "i"` is a constraint. Each constraint indicates a node of the AST that the path must go through.

The modifier `cStrict` requires the nesting to be immediate, for example `[cFor "i"; cStrict; cFor "j"]` requires the for-loop on "j" to appear as direct child in the body of the for-loop on "i", whereas [cFor "i"; cFor "j"]` allows e.g. another intermediate loop to appear between the loop on "i" and that on "j".

Targeting by the string representation is also possible, at the level of instructions or expressions. For example, `[cFor "i"; sInstr "a++"]` targets the instruction `a++` inside the for-loop on "i". Matching is by default on substrings; operations for regular-expressions matching are also available.

By default, a target expects to resolve to exactly one path. The modifier `nbMulti` allows multiple targets. For example, `[nbMulti; cFor "i"]` may point at one or several for-loops index with "i". `nbAny` allows any number of occurrences, including zero. `nbExact 3` checks that the number of occurrences found is exactly 3.

The constraints are defined in the files `constr.ml` and `target.ml`. The resolution of targets is implemented in `constr.ml`. The high-level operators involving targets are implemented in `target.ml` and are described next.

- `Target.iter (tr : path -> unit) (tg : target) : unit` applies an operation `tr` (performing side-effects) at all paths that correspond to the target `tg`.

- `Target.apply_at_target_paths (tr : trm -> trm) (tg : target) : unit` applies a local transformation `tr` at all paths that correspond to the target `tg`.

- If you already have a resolved path and want to apply a local transformation, you can call `Target.apply_at_path`.

Some transformations such as `Instr.insert` need to aim not at one AST node but at a point in-between two instructions. The modifiers `tBefore` and `tAfter` may be placed in the target, e.g. `[tBefore; sInstr "i++"]`.
The modifiers `tFirst` and `tLast` are also available. When such modifiers are used, the path produced by the target resolution mechanism is represented as `[dir1; dir2; .. ; dirN; Dir_before i]`, where the direction `dirN` reaches a sequence, and where `i` denotes the index inside this sequence at which, e.g., the insertion operation should be performed. The syntax `let (pseq,i) = Path.extract_last_dir_before p in` may be used to extract from such a path the path to the sequence `pseq = [dir1; dir2; .. ; dirN]` and, separately, the index `i`.

# Trace

An OptiTrust script consists of a sequence of steps. Each step modifies the "current AST" and produces an updated AST. All the intermediate ASTs are stored in "the trace" (type `trace` in file `trace.ml`).

Steps are materialized in proof scripts by the `!!` symbol. When executed interactively, the script is interrupted when reaching the first `!!` whose location is further than the cursor location in the user's editor.

The set of steps is used to compute the diffs that are produced as output.

# Implementation of transformations

OptiTrust provides a library of transformations. Additionally, the user may define custom transformations, either by implementing them directly at the AST-level, or, preferably, by invoking a combination of existing transformations.

[TODO: For historical reasons, a transformation on a loop is implemented in three files: `loop_core.ml`, `loop_basic.ml` and `loop.ml`. The work-in-progress plan is to only use `loop_basic.ml` and `loop.ml`.]

Only the `*_basic.ml` files are meant to contain functions that directly produce pieces of AST, that is, call smart constructors such as `trm_for`. The higher-level files such as `loop.ml` are meant to contain only to combinations of calls to transformations from the "basic" level. This two-layer stratification is meant to (1) avoid circular dependencies between transformation libraries, and (2) isolate the trusted computing base.

When a transformation produces a fresh piece of AST using smart constructors, these pieces of AST are generally untyped, that is, they do not carry type information. However, there are transformations that do depend on types. In order to restore types, OptiTrust provides the operation `reparse_after tr`, where `tr` is a transformation of type `target->unit`. [TODO: in the future, we will provide an efficient incremental retyping].


# Example transformation: modifying the AST at a given target

FIXME: Remove usage of Target.apply

Example: `Marks_basic.add (m : mark) (tg : target) : unit`.
This transformation attaches a mark named `m` to the nodes at the paths targeted by `tg`.
This function can be implemented as:
```
Target.apply (fun fullterm p ->
    Path.apply_on_path (fun subterm -> trm_add_mark m subterm) fullterm p
   ) tg
```

This pattern is common, and can be factored out using `Target.apply_at_target_paths (transfo : trm -> trm) (tg : target) : unit` as follows:
```
Target.apply_at_target_paths (fun t -> trm_add_mark m t) tg
```
where `t` denotes the targeted subterm.


# Example transformation: modifying the AST at a surrounding node

FIXME: Remove usage of Target.apply

Example: `Sequence.delete (tg : target) : unit`.
This transformation removes the targeted instruction from its surrounding sequence.
The complication is that we need to update the surrounding sequence.
This function can be implemented with help of
`Path.index_in_seq (p : path) : int * path`, which returns the index in the sequence
and the path to the surrounding sequence.
The functions `trm_seq_inv` is used to extract the list (technically a list augmented with
marsk, of type `mlist`) of instructions in the sequence.
The function `trm_inv ~error:..` is used to process the error that may arise in case the
surrounding node is not a sequence.
The function `MList.remove i 1` removes one element from the list of instructions.
The function `trm_seq` is used to build the updated sequence, with the updated list of instructions.
The function `trm_like` is a smart constructor for preserving the annotations and cached information
associated with the original term.

```
Target.apply (fun fullterm p ->
    let i,p_seq = Path.index_in_seq p in
    Path.apply_on_path (fun t_seq ->
      let instrs = trm_inv ~error:"expected a sequence" trm_seq_inv t_seq in
      let instrs2 = Mlist.remove i 1 instrs in
      trm_like t_seq (trm_seq instrs2)
     ) fullterm p_seq
   ) tg
```

# Example transformation: a "combi" transformation that invokes basic transformations

Consider the loop tiling transformation, whose purpose is to change
`for (int i = 0; i < n; i++) { body(i); }` into
`for (int bi = 0; bi < n; bi+=B) { for (int i = bi; i < min(n,bi+B); i++) { body(i); } }`.

Assume this transformation is implemented by
`Loop_basic.tile ?(index : var = "b${id}") (tile_size : trm) (tg : target) : unit`.
where `index` is an optional argument, whose default value is the string "b${id}".
In that index argument, the special token named "${id}" is replaced with the index
variable of the original loop targeted by the transformation. For example, a tiling
applied to `for (int i..)` introduces a loop `for (int bi...`. The user may
override the default argument using the syntax `Loop_basic.tile ~index:"j" tile_size tg`.

We next describe how to implement a higher-level transformation called `Loop.tile_zero`, that ensures the inner loop to start at zero. [TODO: it is not named like that in our library.]
Concretely, it transforms:
`for (int i = 0; i < n; i++) { body(i); }` into
`for (int bi = 0; bi < n; bi+=B) { for (int i = 0; i < min(n-bi,B); i++) { body(i); } }`.
This transformation is achieved by combining a `Loop_basic.tile` followed with a `Loop.shift` operation. It is implemented as shown below, where `target_of_path p` converts a path `p` of type `path` into a target of type `target`, and where `Path.to_inner_loop p` converts a path `p` into the path to the single loop that appears inside the body of the loop at path `p`.

```
let tile_zero ?(index : var = "b${id}") (tile_size : trm) (tg : target) : unit =
  Target.iter (fun t p ->
    reparse_after (Loop_basic.tile ~index tile_size) (target_of_path p);
    Loop.shift StartAtZero (target_of_path (Path.to_inner_loop p));
    ) tg
```

Rather than taking the argument `tg` and passing it to `Target.iter`, we prefer writing the code in "partial application" style, as follows:

```
let tile_zero ?(index : var = "b${id}") (tile_size : trm) : Transfo.t =
  Target.iter (fun t p ->
    reparse_after (Loop_basic.tile ~index tile_size) (target_of_path p);
    shift StartAtZero (target_of_path (Path.to_inner_loop p));
    )
```
where `Transfo.t` is a shorthand for the type `target -> unit`.


