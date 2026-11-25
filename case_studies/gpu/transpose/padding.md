Random notes about padding:
- This is a matrix of tiles. And each tile is accessed independently. The quantification on each is irrelevant
- Here add size one. What would it change? Might need to change the index? I think so yeah
- `MSIZE H/32; W/32; 32; 32` --> instead `H/32; W/32; 32; 33`
- What changes: the size of each tile is of one more column. The last column will never be accesses then. So I dont need to extract anything in fact, except all of the mentions of mindex. And I could make the transformation so that I touch a specific dimension.

- Right after the allocation, the resource would be something like :
`for i1 in 0..+d1 -> for i2 in 0..+d2 -> for i3 in 0..+(d3 + pad) -> for i4 in 0..+d4 -> &a[MINDEX(d1, d2, d3 + pad, d4, i1, i2, i3, i4)] ~~> UninitCell`
but everyone needs the same thing but without padding.
-----> Add the weakening of the resource by hand in ghost? Maybe with a focus or a magic wand?
To get: 
```for i1 in 0..+d1 -> for i2 in 0..+d2 -> ((for i3 in 0..+d3 -> for i4 in 0..+d4 -> &a[MINDEX(d1, d2, d3 + pad, d4, i1, i2, i3, i4)] ~~> UninitCell) * (for i3 in d3..+pad) -> for i4 in d4 -> &a[MINDEX(d1, d2, d3 + pad, d4, i1, i2, i3, i4)] ~~> UninitCell))```
equivalent to :
```for i1 in 0..+d1 -> for i2 in 0..+d2 -> (for i3 in 0..+d3 -> for i4 in 0..+d4 -> &a[MINDEX(d1, d2, d3 + pad, d4, i1, i2, i3, i4)] ~~> UninitCell) * ```
```for i1 in 0..+d1 -> for i2 in 0..+d2 -> (for i3 in d3..+pad -> for i4 in 0..+d4 -> &a[MINDEX(d1, d2, d3 + pad, d4, i1, i2, i3, i4)] ~~> UninitCell) ```

No need for a specific weakening. Simply split cleanly.
To split (without new ghosts):
  - write n-1 for loops
  - split inside the specific for loop, with split (begin, end) * (end, padding)
  - exit loop, then split (different ghost)

Question : how do I represent a normal star?
Can I have something of the shape : for _ in r -> (H1 * H2), and then transform it into "(for _ in r -> H1) * (for _ in r -> H2)" ?
Seems reasonable. But how do I enter the inside, since I need a for? 
Thomas tells me that this is something we do often, but I dont think I can write it as a ghost for that reason. And I don't know how to write it as a group?
I could add a new operator '*', that can be translated into a non indexed group? Or into a group where I replace the variable. (for later)

For the moment, All I need to do is split the resource from a group to the other. 
```
for i1
  for i2
    for i3 + padding
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
```
==> // loop split
```
for i1
  for i2
    for i3 in 0..+r3
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
    for i3 in r3..+padding
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
```
==> // loop fission, around the two.  
```
for i1
  for i2
    for i3 in 0..+r3
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
  for i2
    for i3 in r3..+padding
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
```
==> 
```
for i1
  for i2
    for i3 in 0..+r3
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
for i1
  for i2
    for i3 in r3..+padding
      "for i4 in 0..+r4 -> &p[i1][i2][i3][i4] ~> UninitCell"
      no-op;
```

The padded part will be accessible, starting from the transformation. As long as it preserves semantics this seems fine.


[Question: the goal of transformations is to change the code, while fitting inside the specs basically. But how do we know that we fit the specs.
When does a modification risk to cause a problem? We WILL have a problem with the fission, since it does recomputation, but our temporary resource is wrong. Since it has excess padding inside. How do I go around this? 

Maybe if I understand what the loop fission does inside I won't have an issue. And I still need to resolve that problem of "loop.split_range", which I don't know if it exists. -- Ok it does exists, and it does exactly what I want. 
]



Looking for : either a weakening, or an extract.

API : 
  -> target (in the end) -- Should target the allocation, we assume that the usage will strictly be the same
  -> number of dimensions + dimension index
    -> (or maybe only the dimension index, and I'll guess the rest) -- I'll look for any "MSIZEn" or "MINDEXn", and if I find it I verify whether the dimension fits. 
    -> I could also assume that there are no accesses to the same array under different logic representations, would be weird. So no several MINDEX of different dimensions (seems reasonable) 
  -> 

Unit tests :
  -> Pad by one and read inside
  -> Pad by more than one
  -> Pad by a negative value in a case this makes sense
  -> Pad then write inside (should be the same?)
  -> Pad on different arrays, by different amounts, and access them?
