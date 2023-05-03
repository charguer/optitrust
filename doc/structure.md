
# Overall architecture

- `ast.ml`

A program is represented by its abstract syntax tree (AST). It corresponds to an imperative lambda-calculus, in which there is no notion of left-value nor of mutable variables. (These C features are encoded, as described further on.)

- The type `trm` describes such an ast.
  + The `desc` field of type `trm_desc` indicates the type of the term, for example `Trm_if` describes a conditional.
  + The `annot` field stores meta data, including C display styles (e.g., `*p.f` vs `p->f`).
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

By default, a target expects to resolve to exactly one path. The modifier `nbMulti` allows multiple targets. For example, `[nbMulti; cFor "i"]` may point at one or several for-loops index with "i". `nbAny` allows any number of occurrences, including zero. `nbExact 3` checks that the number of occurrences found is exactly 3.

The constraints are defined in the files `constr.ml` and `target.ml`. The resolution of targets is implemented in `constr.ml`. The high-level operators involving targets are implemented in `target.ml` and are described next.

- `Target.apply (tr : trm -> path -> trm) (tg : target) : unit` applies a transformation `tr` at all paths that correspond to the target `tg`. The transformation `tr` takes as first argument a full ast `t`, and as second argument the path `p` pointed at by target. The arguments `t` and `p` may be, e.g., exploited by `apply_on_path`.

- `Target.iter (tr : trm -> path -> unit) (tg : target) : unit` applies an operation `tr` (performing side-effects) at all paths that correspond to the target `tg`. Compared with `Target.apply`, the transformation `tr` produces a result of type `unit` instead of producing a new `trm`.