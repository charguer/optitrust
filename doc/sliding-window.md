reduce(M: 'a monoid, range, f: int -> 'a) parallel != sequential reduceSeq
for_simpl(range, f) = reduce((), range, f)
range(a, b, s)
make_val_monoid(zero: A, op: (A, A) -> A)
- zero neutral
- op associative

when range positive:
acc = zero
for i in range:
  acc = op(acc, f(i))

can also consider groups:
make_val_group(zero: A, add: (A, A) -> A, sub: (A, A) -> A, neg: A -> A)

reduce(M, range(a, b, 1), f) = M.op(reduce(M, range(a, c, 1), f), reduce(M, range(c, b, 1), f))
- a <= c <= b
- also possible with negative ranges if negation operator and commutative group:
  reduce(G, range(a, c, 1), f) when c < a: [convention: a excluded, c included]
  = G.neg(reduce(G, range(c, a, 1), f))

reduce(G, range(a, b, 1), f) = G.sub(reduce(G, range(c, b, 1), f), reduce(G, range(c, a, 1), f))

reduce(M, range(a, a, 1), f) = M.zero
reduce(M, range(a, a+1, 1), f) = f(a)

array_segment_reduce(M, range, array) = reduce(M, range, fun i -> array[i])
array_segment_simple_reduce(M, a, b, array) = array_segment_reduce(M, range(a, b, 1), array)

monoid_add = make_val_monoid(0, +)

---

int x = new (reduce(M, range, f))
--->
int x = M.zero; for i

+ Loop.hoist
---

for i = 0 to n
  T1
  out[i] = reduce(monoid_add, in, i, i+k)
  T2

correct when:

for i = 0 to n
  __parallel_reads(for i in r -> in ~> Cell);
  T1
  with_focus_subrange (i..i+k) {
    out[i] = reduce(monoid_add, in, i, i+k) # ou i, k
  }
  T2

--> (bind)

for i = 0 to n
  __parallel_reads(for i in r -> in ~> Cell);
  T1
  with_focus_subrange (i..i+k) {
    alloc s
    s = reduce(monoid_add, in, i, i+k)
    out[i] = s
  }
  T2

--> (Instr.move_in_seq + Loop.hoist)

alloc s
for i = 0 to n
  __parallel_reads(for i in r -> in ~> Cell);
  T1
  with_focus_subrange (i..i+k) {
    s = reduce(monoid_add, in, i, i+k)
    out[i] = s
  }
  T2

--> (If.insert, )

alloc s
for i = 0 to n
  __modifies("s ~> Cell");
  T1
  if i = 0
    with_focus (0..0+k) {
      s = reduce(monoid_add, in, 0, 0+k)
    }
  else
    // old s = reduce(monoid_add, in, i-1, i+k-1)
    // s = reduce(monoid_add, in, i, i+k)
    //   = reduce(.., i+k-1, i+k) add reduce(.., i-1, i+k-1) sub reduce(.., i-1, i)
    //   = reduce(.., i+k-1, i+k) add reduce(.., i-1, i+k-1) add reduce(.., i, i-1) [neg range]
    //   = in[i+k-1] add old s sub in[i-1]
    with_focus (i+k-1, i-1) {
      s += in[i+k-1] - in[i-1]
    }
  out[i] = s
  T2

isolate_first_iteration, if_elim_true, if_elim_false:

alloc s = 0
for i = 0 to n
  contract

  T1
  if i = 0
    focus { s = reduce(monoid_add, in, 0, 0+k) }
  else
    focus { s += in[i+k-1] - in[i-1] }
  out[i] = s
  T2

-->

- cond is formula convertible
- simpl 0 = 0 --> true
- i = 0 with i in 1..n --> false

__requires(n > 0 || out[0]) (ghost ensures)
alloc s = 0
T1[i := 0]
focus { s = reduce(monoid_add, in, 0, 0+k) }
out[0] = s
T2[i := 0]
for i = 1 to n
  contract
  
  T1
  focus { s += in[i+k-1] - in[i-1] }
  out[i] = s
  T2
