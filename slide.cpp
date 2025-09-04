void rowSum(const int w, const uint8_t* S, uint16_t* D, const int n, const int cn) {
  __requires("w >= 0, n >= 1, cn >= 0");
  __reads("S ~> Matrix2(n+w-1, cn)");
  __writes("D ~> Matrix2(n, cn)");


    for (int c = 0; c < cn; c++) { // foreach channel
      __sreads("S ~> Matrix2(n+w-1, cn)");
      __xwrites("&D[MINDEX2(n, cn, i, c)] ~> Cell");

      __ghost(assume, "is_subrange(i..i + w, 0..n + w - 1)"); // TODO: solve


    for (int i = 0; i < n; i++) { // for each pixel
        __xwrite(D [i] ~> reducefromto(i, i+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]) )

      uint16_t s = (uint16_t)0;
      for (int k = i; k < i+w; k++) {
        __invariant( s = reducefromto(i, k, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]));
        __ghost(in_range_extend, "k, i..i+w, 0..n+w-1");
        __GHOST_BEGIN(focus, ro_matrix2_focus, "S, k, c");
        s += (uint16_t)S[MINDEX2(n+w-1, cn, k, c)];
        __GHOST_END(focus);
      }
      D[i] = s



--->

      uint16_t s = (uint16_t)0;
      for (int k = i; k < i+w; k++) {
        __invariant( s = reducefromto(i, k, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]));
        __ghost(in_range_extend, "k, i..i+w, 0..n+w-1");
        __GHOST_BEGIN(focus, ro_matrix2_focus, "S, k, c");
        s += (uint16_t)S[MINDEX2(n+w-1, cn, k, c)];
        __GHOST_END(focus);
      }
      D[0] = s
      ctx = (D [0] ~> reducefromto(0, 0+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]) )

  ----
  Slide_basic.intro
    ~fill:"D[i]" ~from:0 ~to:n
    ~source:"S[i]"
    (cFor "i") <-- block à remplacer
----
  Slide.intro   // info read in xwrite
    (cFor "i") <-- block à remplacer
    ----

    ctx = (D [0] ~> reducefromto(0, 0+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]) )
    ctx = for i in 1..n   D [i] ~> reducefromto(i, i+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]) )





    for (int i = 1; i < n; i++) { // for each pixel
        __xwrite(D [i] ~> reducefromto(i, i+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]) )

      uint16_t s = (uint16_t)0;
      for (int k = i; k < i+w; k++) {
        __invariant( s = reducefromto(i, k, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]));
        __ghost(in_range_extend, "k, i..i+w, 0..n+w-1");
        __GHOST_BEGIN(focus, ro_matrix2_focus, "S, k, c");
        s += (uint16_t)S[MINDEX2(n+w-1, cn, k, c)];
        __GHOST_END(focus);
      }
      D[i] = s


    --->

    s0 = 0
    for (k = 0 to w)

    D[0] = s0


    for (int i = 1; i < n; i++) { // for each pixel

        _spreserves(for  0 <= j < i, D [j] ~> reducefromto(j, j+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]
             )
        _spreserve(for   i < j < n,    D[j] ~> Uninit)

        __xwrite(D [i] ~> reducefromto(i, i+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]) )
        D[i+1] = D[i] + S[i+w] - S[i]
   D[i] + S[i+w] - S[i] ~~> reducefromto(i+1, j+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]


        _spreserves(forall j < i, D [j] ~>
             )



  }
}
ghost
 i > 0
_requires(forall j < i, D [j] ~> reducefromto(j, j+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)]
_ensures( D[i] + S[i+w+1] - S[i] = reducefromto(i+1, i+1+w, fun k ->
            S[MINDEX2(n+w-1, cn, k, c)])
