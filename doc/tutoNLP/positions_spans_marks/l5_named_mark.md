# l5_named_mark

Difficulty: Level 5

Request:

```text
target the kernel_sequence mark
```

Expected target:

```ocaml
[cMark "kernel_sequence"]
```

This target is valid only when the current AST or trace contains an OptiTrust
mark named `kernel_sequence`. If only the plain `.cpp` source is available, the
assistant should say that mark validation needs the transformed AST or trace.
