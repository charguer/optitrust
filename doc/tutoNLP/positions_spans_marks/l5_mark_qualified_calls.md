# l5_mark_qualified_calls

Difficulty: Level 5

Request:

```text
target every magic_barrier call inside the kernel_sequence mark
```

Expected target:

```ocaml
[nbAny; cMark "kernel_sequence"; cCall "magic_barrier"]
```

If the assistant only receives the plain `.cpp` file and no marked trace or AST,
it should ask for the marked source context before claiming validation.
