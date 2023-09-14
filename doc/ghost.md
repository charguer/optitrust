# Definition of a ghost function

```c
__GHOST(ghost_foo) {
    __require("g: int;");
    __ensures("h: int;");

}
```

# Call to a ghost function

```c
int main() {
    __pure();
    ...

    ghost_foo("g := 3"); __bind("d as k");
    // This is a form of mandatory syntactic sugar for:
    ghost_foo(); __with("g := 3"); __bind("d as k");
}
```

# Definition of a C function with ghost args

```c
int f(int x) {
    __require("a: int;");
    __ensures("b: int;");
    ...
}

void g(int x) {
    __require("c: int; d: int;");
    __ensures("e: int;");
    ...
}
```

# Call to a C function with ghost args

```c
int main() {
    __pure();
    ...

    g(8); __with("c := 3, d := 9"); __bind("d as k");
    g(8); __with("3, 9"); __bind("k"); // positional syntax

    g(__call_with(f(5), "a := 9")); // no __bind in subexpressions
}
```

`__bind` is not implemented yet
