# l5_line_without_source

Difficulty: Level 5

Request:

```text
target the loop on line 10
```

Expected result:

The assistant must not return a final target. It should ask for the source code
or for a semantic identifier such as the loop index, enclosing function, or
surrounding statement.

Validation with `Show.target` is blocked until source context is provided.
