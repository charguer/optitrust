# OptiLambda V1 Syntax

This module defines a standalone textual syntax for the internal OptiTrust AST.
The goal is to print and eventually parse OptiLambda without going through the
C/C++ printer.

## Terms

Variables:

```optilambda
x
```

Function definitions:

```optilambda
fun f[A](x: A, y: B): A [h1, h2] {
	requires h1: x = y;
	produces h2: y = x;

	BODY
}
```

Ghost functions hide the internal `__ghost_ret` return type in the simple display:

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

Bindings:

```optilambda
let x: A = x1
let x = x1
letmut x = 3
```

Blocks:

```optilambda
{
	t1;
	t2;
	x
}
```

All regular block items end with a semicolon. A final returned expression is
printed without the `return` keyword and without a trailing semicolon.

Loops:

```optilambda
for<seq> i in 0..n [h1] {
	BODY
}

for<seq> i in 0..n:2 {
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

Loop directions are omitted in the simple display. A negative step represents a
downward loop, e.g. `for<seq> i in n..0:-1`.

Conditionals:

```optilambda
if (t0) [h1, h2] {
	t1
} else {
	t2
}
```

Loops:

```optilambda
while (t0) [h1] {
	BODY
}
```

Ghosts:

```optilambda
ghost(f(t))
ghost(t)
ghost_begin(x, t)
ghost_end(x)
```

Mutable-variable display:

```optilambda
x
x = 3
t[i][j]
t[i][j] = 3
v.x
{1, 2}
```

The verbose/debug form for indexed accesses may later expose dimensions:

```optilambda
t[i#n][j#m]
```

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

Logical terms follow the existing resource formula syntax used by
`resource_cparser.mly`.
