# Source codes

Source codes in C or in C++ are allowed, but only a subset of these languages is
dealt with.

Constraints on switch statements:
- Cases must end with a break instruction.
- Nested cases must share their entire body. We call them case groups.

Constraint on `const` variables: it is forbidden to use the "address of"
operator on them.

Variables that are not `const` are heap allocated: the corresponding AST use
`const` pointers to such variables. This should be transparent for the user.

