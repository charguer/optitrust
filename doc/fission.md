# Fission with ghost pairs

  // input code
  for i
    focus(i)
    t1
    // fission here
    t2
    unfocus(i)
    
  // output
  for i
    focus(i)
    t1
    unfocus(i)
  for i
    focus(i)
    t2
    unfocus(i)


# Fission with pure ghost

input code:
 for i
    pure(i) // a ghost that produces a pure fact that depends on i
    t1
    // fission here

simplest version:
  for i
    xensures p(i)
    pure(i)
    t1
  for i
    xrequires p(i) // only if p(i) is used in t2, else minimize will remove it
    t2

simple version with isolated pure:
  for i
    xensures pure(i)
    pure(i)
  for i
    xrequires pure(i)
    t1 // only way to isolate t1 in a loop nest
  for i
    xrequires pure(i)
    t2

as a ghost pair:
  // involves duplication of pure(i) however, avoids xensures
  for i
    pure(i)
    t1
  for i
    pure(i)
    t2

more general case as a ghost pair:
input:
  for i
    p1 // pure
    t1 // term
    p2
    t2
    <--split point
    p3
    t3

=> fissions duplicates pure facts before split point
output:
  for i
    p1
    t1
    p2
    t2
    // forget p1, p2, implicit or explicit?
    // similar to ghost pairs that are closed
    <--split point
    p1 // pure copied
    p2 // pure copied
    p3
    t3
then basic split

auxiliary question: are we able to convert via transfo between all these form


other case:
input code:
  for i
    pure // a ghost that produces a pure fact that does not depend on i
    t1
    t2

should be hoisted to:
  pure
  for i
    t1
    t2

what about swap on non-perfect nest (perfect only modulo ghost):

  for i
    pure(i)
    for j
      t

  => swap

  for j
    for i
      pure(i)
      t


# Both pure and pair ghosts

input:
  for i
    pure(i)
    focus(i)
    for j
      t
    unfocus(i)

just split before swap:
  for i
    pure(i)
    focus(i)
  for i // ready for swap
    for j
      t
  for i
    unfocus(i)

now, should we duplicate the pure?
  for i
    pure(i)
    focus(i)
  for i // still ready for swap, but pure remains
    for j
      pure(i)
      t
  for i
    pure(i) // will certainly be removed by minimize, because unfocus is self-contained
    unfocus(i)



## Proof that swap is a general case of fission/fusion

input:
  for i = 1 à 2
    for j
      t
      
unroll:
  for j
    t(1,j)
  for j
    t(2,j)

fusion:
  for j
    t(1,j)
    t(2,j)

roll:
  for j
    for i = 1 à 2
     t(i,j)


## Why not doing the same for ghost pair

example explained on matrix processing:
  M : matrix
  for i
    focus M on line i, gives stars_j M[i][j]
    for j
      xmodifies M[i][j]
      M[i][j]++

fission before swap:
  for i
    focus M on line i, gives stars_j M[i][j]
  for i // perfect for swap
    xmodifies M[i][*]  // shorthand for stars_j M[i][j]
    for j
      xmodifies M[i][j]
      M[i][j]++

move-in:
  M : matrix
  for i
    for j
      modifies M // no longer parallel
      focus M on line i, gives M[i][*]
      focus M[i][*] on column j, gives M[i][j]
      M[i][j]++

This is problematic since we lose parallelism and we also need to care about detaching focus on the loop.
