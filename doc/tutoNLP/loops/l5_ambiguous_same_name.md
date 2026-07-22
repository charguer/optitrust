# l5_ambiguous_same_name

Difficulty: Level 5

Request:

```text
target the loop i
```

Expected result:

The assistant must not return a final target. It should ask which enclosing
function is intended and show these alternatives:

```ocaml
[cFunBody "init"; cFor "i"]
[cFunBody "step"; cFor "i"]
```

Validation is blocked until the user chooses one candidate.
