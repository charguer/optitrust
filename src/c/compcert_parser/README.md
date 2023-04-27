
DO NOT EDIT THE FILES IN THIS DIRECTORY.

They are imported from a patched version of CompCert
https://github.com/charguer/CompCert
to support several language extensions.


# Size of arrays

In C99, array sizes must be constants, they cannot refer to variables.
We relax this restriction with flag `Elab.allow_variables_as_array_size`.

We also extend the syntax tree to record the original expressions that
correspond to the size, in addition to the computed value. Both are printed
by `Cprint.`. For example, the source code:

```
const int CHUNKSIZE = 128;
int t[CHUNKSIZE];
int u[2*CHUNKSIZE+1];
```
is printed back as:

```
int const CHUNKSIZE = 128;
int t[CHUNKSIZE /*=128*/];
int u[2 * CHUNKSIZE + 1 /*=257*/];
```

In the future, we could use options to choose between printing only
computed values, or only original expressions, or both, in which case
we need to specify which one of the two goes in the comment.


# Compound initializers in return statements

In C99, a return statement can take as argument only an expression,
not an initializer. We relax this restriction, by elaborating the
initializer using the return type of the function. For example, in:

```
typedef struct { int x, y; } vect;

vect f() {
   return { 1, 2 };
}
```

the initializer `{ 1, 2 }` is elaborated at type `vect`.



# Default name for typedef on anonymous struct

In the original elaborator, the following definition is correctly processed.

```
typedef struct { int x; int y; } vect;
```

However, `CPrint` prints it incorrectly as:

```
struct ;
struct   { int x; int y; };
typedef struct  vect;
```

We fixed this by using the name of the `typedef` as name for the `struct`.
We now obtain:

```
struct vect;
struct { int x; int y; } vect;
typedef struct vect;
```

In case of multiple typedef at once (yes, it is allowed!), we consider
arbitrarily the first name. It seems to make to no visible difference
to the user. For example, for:

```
typedef struct { int x; int y; } vect1, vect2;
```

The printed output is:
```
struct vect1;
struct { int x; int y; } vect1;
typedef struct vect1 vect1;
typedef struct vect1 vect2;
```

Note that `gcc -std=c99 c_small_test.cpp` accepts typedef recursive definitions
without require the `struct` keyword.

```
typedef struct list {
  int head;
  list* tail; // CompCert requires "struct list* tail", it seems to follow the standard
} list;
```


# Implicit return on main

The elaborator adds a `return 0;` at the end of the main (even if there is already a return).
We introduce the option `Elab.generate_implicit_return_on_main` to disable this feature.


# The __func__ bindings

The elaborator generates assignment to the special `__func__` array.
We introduce the option `Elab.generate_static_func_names` to disable this feature.


# Location for expressions

We've added `eloc` as a location field in expressions, in `C.mli`.


# For loop with C++ binding scope

The for loop construct

```
for (int i = 0; ; i++) { }
```

gets elaborated in CompCert as:

```
{
   int i = 0;
   for (/*nothing*/; 1; i++) { }
}
```

We've added a mode `Elab.keep_for_loops_untransformed` to keep the declarations
in the loop initializer statement, and have CPrint get back the original form:

```
for (int i = 0; /*nothing*/; i++) { }
```

The implementation supports the various forms of for-loops.


# Smart constructors for the C language moved to C.ml

The file `Cutil.ml` depends on `Cprint.ml`, and only for one error message in the
`sizeof` function. I felt instead the need for Cprint to depend on Cutil, to
access auxiliary functions for manipulating expressions.

We need access from Cprint to `no_exp` (dummy expression used in the size
annotation for `TArray`), as well as to `missing exp_missing_for_loop_conditional`
(used to keep for-loops in their original form).

Thus, I created a file C.ml, containing the contents of C.mli, plus the
smart constructors extracted from Cutil.ml, and the new smart constructors
which we introduced.

Btw, `dune` seem to require an .ml file for every .mli file, so it's probably
a good idea to have a `C.ml` file anyway.


# Reference types (only supported as local variables)

We introduce support for a convenient C++ feature: references as local variables.
We do not (yet) support references as global variables or as function arguments.
For example:

```
int y = 0;
int& x = y;
```

The type `int&` is represented as `TRef (TInt (IInt,[]))`.
At the moment, as a naive yet sufficient implementation, from the perspective of the elaboration,
the definition `int& x = y` is processed as if `int x = y` was written, in the sense that the
type `int` is registered for `x` in the environment. The statement `Sdecl` still carries
the `TRef` type, however, to keep track that it consists of a reference declaration.



# Other questions about CompCert's parser/elaborator

The associativity mode `RtoL` does not seem to be ever exploited.

Why are forward definitions such as `struct vect;` systematically introduced?

In the file Makefile.expr, the rule for "depend" is missing dependencies, because
the rule only depends on `$(GENERATED)`. Suggested fix:

```
ALL_ML := $(foreach d,$(DIRS),$(wildcard $(d)/*.ml))
ALL_MLI := $(foreach d,$(DIRS),$(wildcard $(d)/*.mli))
depend: $(GENERATED) $(ALL_ML) $(ALL_MLI)
	@echo "Analyzing OCaml dependencies"
	@$(OCAMLDEP) $(ALL_ML) $(GENERATED) >.depend.extr || { rm -f .depend.extr; exit 2; }
	@$(OCAMLDEP) $(ALL_MLI) $(GENERATED) >>.depend.extr || { rm -f .depend.extr; exit 2; }
```


`C2C.ml` was quite confusing a name for me at first. Suggested: `C2Csyntax.ml`.

It would be nice to have precise locations for all expressions. I can do the
necessary change at some point in the future. One question is whether all data
types should also carry locations, in particular "typ" and "init" and "field",
etc. If we want a nice, general-purpose parser, this is needed.

It would be nice to carry in locations not just the line numbers but also the
start and end column. Can we easily extract end columns from the parser?

