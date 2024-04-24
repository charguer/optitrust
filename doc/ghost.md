# Definition of a ghost function

```c
__GHOST(foo) {
    __require("g: int");
    __ensures("h: int");

}
```

# Call to a ghost function

```c
int main() {
    __pure();
    ...

    __ghost(foo, "g := 3"); __bind("h as k");
    // This is syntactic sugar for:
    foo(); __with("g := 3"); __bind("h as k");
}
```

# Definition of a C function with ghost args

```c
int f(int x) {
    __require("a: int");
    __ensures("b: int");
    ...
}

void g(int x) {
    __require("c: int, d: int");
    __ensures("e: int");
    ...
}
```

# Call to a C function with ghost args

```c
int main() {
    __pure();
    ...

    g(8); __with("c := 3, d := 9"); __bind("e as k");
    g(8); __with("3, 9"); __bind("k"); // positional syntax

    g(__call_with(f(5), "a := 9")); // no __bind in subexpressions

    // bind possible for a function with a return that is bound in a declaration
    int x = f(5); __with("a := 9"); __bind("b as t");
}
```

`__bind` is not implemented yet

# Group ghosts combinations

For group tiling and shifting, we define a trusted/verified ghost that operates on the first (outer) group of a formula (e.g. `group_shift`).
To handle multiple nested groups, we define variants that operate on the n-th group of a formula (e.g. `group_shift2`, `group_shift3`, ...).
This is equivalent to using `for` loops but avoids having to write contracts: `group_shift{n+1} = for i { group_shift{n} }`.
This requires definining `n` ghosts for `n` nest sizes, but is not too bad.
To shift the 2nd and 3rd groups, one can use sequential composition: `group2_shift(...); group3_shift(...)`.
This does not require writing contracts so we do *not* built-in ghosts for every possible combination, e.g. `group23_shift = group2_shift; group3_shift`.

Instead of shifting the 2nd and 3rd groups, we could also define ghosts that shift all groups up to the 2nd or 3rd.
This would allow using less ghosts to shift multiple groups in a nest, however this would encourage shifting more groups than necessary.
Because shift no-ops have a syntactic impact on logic formulas, we avoid this solution.

A similar design should probably be used for all group ghosts that operate on the first (outer) group of a formula.

At a metaprogramming level, OCaml scripts can also use these ghost building blocks with more practical APIs for multigroup shifts/tiles, e.g.: `shift [s1, s2, s3]`, `shift [(1, s1); (3, s3)]`.

# Permission mode combinations

Many ghosts are applicable to different permission modes: `R`, `_Uninit(R)`, `RO(f, R)`.
This is the case of group tiling and shifting mentioned earlier.
Currently, we accept defining 3 variants of such ghosts for every permission mode.

In the future, one can imagine that we would support permission polymorphism.
By taking a parameter `P := Full | _Uninit | \x. RO(?f, x)`, a ghost could produce/consume, e.g.: `P(R)`.
