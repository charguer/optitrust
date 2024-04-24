memcopy(p, q, n)/MATRIX1COPY(p, q, n) requires contiguous cells: "i: 0..n -> &p[i] ~> Cell" or "p ~> Array(n)"

MATRIX2COPY(p, q, n, m) requires contiguous cells: "i: a..b -> j: 0..m -> &p[MINDEX(_, m, i, j)] ~> Cell".
In other words "i: a..b -> j: 0..m -> &p[MINDEX(_, m, i, j)] ~> Cell" entails "k: a*m..b*m ..m -> &p[k] ~> Cell".

convenience memcopy variants could support offsets to copy sub-tiles.
