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
	t3;
	return x
}
```

Loops:

```optilambda
for<seq, up>(i = 0, n, 1) [h1] {
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

Loop directions:

```text
up      DirUp
up_eq   DirUpEq
down    DirDown
down_eq DirDownEq
```

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
requires h1: x = y;
produces h2: y = x;
```

Logical terms follow the existing resource formula syntax used by
`resource_cparser.mly`.

