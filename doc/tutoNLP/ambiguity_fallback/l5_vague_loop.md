# l5_vague_loop

Difficulty: Level 5

Request:

```text
target the loop
```

Expected result:

The assistant must not return a final target. It should ask whether the user
means the `i` loop or the `j` loop, and show these alternatives:

```ocaml
[cFor "i"]
[cFor "j"]
```

Validation is blocked until the user chooses a candidate.
