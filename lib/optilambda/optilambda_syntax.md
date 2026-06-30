# OptiLambda Syntax And Representations

OptiLambda is the textual language used to display OptiTrust internal AST terms
without going through the C/C++ printer. The current implementation is a printer
over `Ast.trm`; a parser is planned later, but is not implemented yet.

OptiLambda now supports three synchronized representations of the same AST:

- **Surface**: human-oriented syntax, closest to the intended frontend display.
- **Internal**: explicit internal operations such as `get`, `set`, `ref`, array
  access, and record access, without full type parameters.
- **Fully-Typed Internal**: same explicit structure as Internal, but with type
  parameters when the AST has enough type information.

All three representations describe the same program state. Switching
representations changes only the printed code; it does not change the trace tree,
transformation step, AST, or diff structure.

## Selecting A Representation

The OptiLambda printer style is defined in `optilambda_style.ml`:

```ocaml
type representation =
  | Surface
  | Internal
  | FullyTypedInternal
```

String names used by the framework and trace server are:

```text
surface   Surface
internal  Internal
typed     Fully-Typed Internal
```

The framework command-line flags are:

```bash
-print-optilambda-syntax
-optilambda-repr surface
-optilambda-repr internal
-optilambda-repr typed
```

`-print-optilambda-syntax` defaults to `surface`.

The old near-C internal syntax flag remains separate:

```bash
-print-optitrust-syntax
```

## Surface Representation

Surface syntax is optimized for reading traces and diffs. It hides low-level
reference operations when possible.

Variables and mutable reads:

```optilambda
x
```

Mutable assignments:

```optilambda
x = 3
```

Mutable declarations:

```optilambda
letmut x = 3
letmut x
```

Bindings:

```optilambda
let x: A = x1
let x = x1
letmut x = 3
```

Array and matrix-style access:

```optilambda
t[i]
t[i][j]
t[i][j] = 3
```

Record access and record literals:

```optilambda
v.x
{1, 2}
```

Function definitions:

```optilambda
fun f[A](x: A, y: B): A [h1, h2] {
  requires h1: x = y;
  produces h2: y = x;

  BODY
}
```

Ghost functions hide the internal `__ghost_ret` return type in Surface syntax:

```optilambda
ghost fun assert_prop() {
  requires P: Prop,
           proof: P;
  ensures proof: P
}
```

Function calls with contract arguments and returned contract bindings:

```optilambda
f(x1, y1)[h1 := g1, h2 := g2][z : h2]
```

Blocks:

```optilambda
{
  t1;
  t2;
  x
}
```

Regular block items end with a semicolon. A final returned expression is printed
without the `return` keyword and without a trailing semicolon.

Loops:

```optilambda
for<seq> i in 0..n [h1] {
  BODY
}

for<seq> i in range(0, n, 2) {
  BODY
}
```

Loop modes:

```text
seq          Sequential
par          Parallel
gpu_thread   GpuThread
magic_thread MagicThread
```

Loop directions are omitted in Surface syntax. A negative step represents a
downward loop, for example:

```optilambda
for<seq> i in range(n, 0, -1)
```

Resource groups are displayed with the same surface range notation when they
wrap a `range(...)` iterator:

```optilambda
for i in 0..n {
  items(i)
}

for i in range(0, n, step) {
  items(i)
}
```

Desugared read-only and write contracts are recovered in Surface syntax when
the consumed and produced resources clearly form the expected pair:

```optilambda
reads h: H
writes h: H
```

Conditionals and loops:

```optilambda
if (t0) [h1, h2] {
  t1
} else {
  t2
}

while (t0) [h1] {
  BODY
}
```

Ghost terms:

```optilambda
ghost(f(t))
ghost(t)
ghost_begin(x, t)
ghost_end(x)
```

## Internal Representation

Internal syntax exposes the operations hidden by Surface syntax, but omits full
type parameters.

Mutable reads:

```optilambda
get(x)
```

Mutable writes:

```optilambda
set(x, v)
```

Mutable declarations:

```optilambda
let x = ref(v)
let x = ref_uninit()
```

Array access:

```optilambda
get(t [+] i)
set(t [+] i, v)
```

Record access:

```optilambda
get(v [.] x)
set(v [.] x, value)
```

Nested accesses remain explicit:

```optilambda
get((get(p [.] pos)) [.] x)
```

This representation is useful when debugging how readable Surface syntax maps
back to lower-level OptiTrust operations.

## Fully-Typed Internal Representation

Fully-Typed Internal syntax exposes the same operations as Internal, but adds
type parameters where available.

Mutable reads:

```optilambda
get<A>(x)
```

Mutable writes:

```optilambda
set<A>(x, v)
```

References:

```optilambda
ref<A>(v)
ref_uninit<A>()
```

Array access:

```optilambda
get<A>(Array_Access<A>(t, i))
set<A>(Array_Access<A>(t, i), v)
```

Record access:

```optilambda
get<A>(Record_Access<A>(v, x))
set<A>(Record_Access<A>(v, x), value)
```

The quality of this output depends on the type information available in the AST.
When a type is missing, the printer falls back to a conservative textual form.

## Contracts

Function clauses:

```text
requires
consumes
ensures
produces
reads
writes
pure
modifies
preserves
```

Loop/body clauses:

```text
xrequires
xensures
xreads
xwrites
xmodifies
xpreserves
xconsumes
```

Examples:

```optilambda
requires h1: x = y,
         h2: y = z;
produces h2: y = x;
```

Resource points-to formulas use infix notation in all three representations:

```optilambda
(src ~> Matrix1(length, model))
```

Function contracts recover compact `reads` and `writes` clauses in all three
representations when the desugared resources match the safe user-facing
patterns:

```optilambda
reads h: H
writes h: H
```

`reads` means that the same fractional read-only permission `_RO(f, H)` is
present in both the precondition and the postcondition. `writes` means that an
uninitialized resource is consumed and the initialized resource is produced.
Read-only transformations that change the resource shape, split or join
fractions, or produce a `Wand(...)` stay explicit as `consumes` / `produces`.

Logical terms follow the existing resource formula syntax used by
`resource_cparser.mly`.

## Diff And Trace Integration

The framework can generate C/C++ plus the three OptiLambda representations for
diffs and traces.

Generated step-diff sidecar files include:

```text
foo_before.opti
foo_after.opti
foo_before_surface.opti
foo_after_surface.opti
foo_before_internal.opti
foo_after_internal.opti
foo_before_typed.opti
foo_after_typed.opti
```

Standalone trace data includes representation-specific fields:

```text
code_before_optilambda_surface
code_after_optilambda_surface
diff_optilambda_surface
code_before_optilambda_internal
code_after_optilambda_internal
diff_optilambda_internal
code_before_optilambda_typed
code_after_optilambda_typed
diff_optilambda_typed
```

Legacy fields are kept as Surface aliases:

```text
code_before_optilambda
code_after_optilambda
diff_optilambda
```

The trace server accepts:

```text
syntax=cpp
syntax=optilambda&repr=surface
syntax=optilambda&repr=internal
syntax=optilambda&repr=typed
```

The shared HTML diff and trace views can switch between:

```text
C/C++
Surface
Internal
Fully-Typed
```

## Current Limitations

- The printer exists; the OptiLambda parser is future work.
- Surface HTML metadata exists in `optilambda_html.ml`, but richer typed HTML
  rendering can still be improved.
- VS Code integration should keep `.opti` as one language id. Surface, Internal,
  and Fully-Typed Internal are views of the same language, not separate VS Code
  languages.
