#include <optitrust.h>
#include <stdatomic.h>

/* void test_faa_for_loop (_Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i=0 ; i<1024 ; i++) { // produces: __ghost(assume, "in_range(i, 0..1024)");
    __strict();
    __sreads("A ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");
  __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, i");
  __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, i");
    int a = atomic_fetch_add(&ns[MINDEX1(1024, i)], A[MINDEX1(1024, i)]);
  __GHOST_END(focusA);
  __GHOST_END(focusns);
  }
} */


/* void test_faa_basic (_Atomic int* ns, const int v) { // ns : typ_atomic(typ_ptr(typ_int))
  __atomic("ns ~> Cell"); // Atomic(a,H)
  int a = atomic_fetch_add(ns, v);
}

void test_array_faa_basic (_Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __atomic("ns ~> Cell");
  __GHOST_BEGIN(focus, ro_matrix1_focus, "A, 5");
    int a = atomic_fetch_add(ns, A[MINDEX1(1024, 5)]);
  __GHOST_END(focus);
}

void test_faa_basic2 (_Atomic int* ns, const int v) { // ns : typ_atomic(typ_ptr(typ_int))
  __atomic("ns ~> Matrix1(1024)"); // Atomic(a,H)
  __GHOST_BEGIN(focus, atomic_matrix1_focus, "ns, 5");
    int a = atomic_fetch_add(&ns[MINDEX1(1024, 5)], v);
  __GHOST_END(focus);
}

void test_array_faa_basic2 (_Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");
  __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, 5");
    __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, 5");
    int a = atomic_fetch_add(&ns[MINDEX1(1024, 5)], A[MINDEX1(1024, 5)]);
  __GHOST_END(focusA);
  __GHOST_END(focusns);
}

void test_faa_for_loop (_Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i=0 ; i<1024 ; i++) { // produces: __ghost(assume, "in_range(i, 0..1024)");
    __strict();
    __sreads("A ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");

  __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, i");
  __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, i");
    int a = atomic_fetch_add(&ns[MINDEX1(1024, i)], A[MINDEX1(1024, i)]);
  __GHOST_END(focusA);
  __GHOST_END(focusns);
  }
} */

/* bool is_in_range(int start, int stop, int i) {
  __pure();
  return (start <= i && i < stop);
} */

void faa_with_for_and_if (_Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i=0 ; i<1024 ; i++) {
    __strict();
    __sreads("A ~> Matrix1(1024)");
    /* __satomic("ns ~> Matrix1(1024)");
 */
    for (int d = -5 ; d < 6 ; ++d) {
      const int j = i+d;
      if (0 <= j && j < 1024) {
        __ghost(assume, "in_range(j, 0..1024)"); // LATER: deduced from if is_in_range

        /* __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, j");
        __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, j");
          int a = atomic_fetch_add(&ns[MINDEX1(1024, j)], A[MINDEX1(1024, j)]);
        __GHOST_END(focusA);
        __GHOST_END(focusns); */
      }
    }
  }
}


/* void test_faa_for_loop2 (int* np, _Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __modifies("np ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i=0 ; i<1024 ; i++) {
    __strict();
    __sreads("A ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");

  for (int d = -5 ; d < 6 ; ++d) {
    const int j = i+d;
    if (is_in_range(0, 1024,j)) {
       __ghost(assume, "in_range(j, 0..1024)"); // LATER: deduced from if is_in_range
      __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, j");
         if (-1 <= d && d <= 1) {
         __GHOST_BEGIN(focusnp, atomic_matrix1_focus, "ns, j");
          np[MINDEX1(1024, j)]= A[MINDEX1(1024, j)];
          __GHOST_END(focusnp);
        }
        else {
          __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, j");
          int a = atomic_fetch_add(&ns[MINDEX1(1024, j)], A[MINDEX1(1024, j)]);
          __GHOST_END(focusns);
        }


      __GHOST_END(focusA);

    }
  }
}}
} */
/* TODO : 1) tiler boucle sur i ci-dessus et color
          2)

  n ~> Matrix

  delocalize

  n[2] ~> Matrix

  arraytovariables? => priorité dernière

  ns
  np

  introduction du if
  insert_if :   t   --->   if tc then t1 else t2
    vérifier que c fait pas d'effets    Resources.trm_is_pure tc

    où tc : estce que j est à distance 1/2 bloc du bloc au plus

  toatomic : atomic type, atomic contract, atomic operation => avant dernier

  Instr.insert   [cMark "test_on_j"; dThen; tFirst]
    => ghost assume j in max(0,bi-B/2)..min(bi+B+B/2,1024)

  parallelize loop:

    for c
      ghost:
          for i in 0..1024 ->  np[i] ~> cell
          for bi -> for i in max(0,bi-B/2)..min(bi+B+B/2,1024) np[i] ~> cell

      parallel for bi
        satomic ns
        modifies for i in max(0,bi-B/2)..min(bi+B+B/2,1024) np[i] -> cell
        for i
          for d
            int j = i + d
            if (j is_in_range 0..1024)
               ghost assume j in 0..1024
              if j is_in_range bi-B/2 .. bi+B+B/2
                ghost assume j in max(0,bi-B/2)..min(bi+B+B/2,1024)


          */


/* void test_faa_with_blocks (int* np, _Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __modifies("np ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int bi = 0; bi < 256; bi++) {
    __strict();
    __sreads("A ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");
    __xmodifies(
        "for i in 0..4 -> &np[MINDEX1(1024, bi * 4 + i)] ~> Cell");
    for (int i = 0; i < 4; ++i) {
      __strict();
      __sreads("A ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");
      __xmodifies("for d in 4*bi-2..4*bi+6 -> &np[MINDEX1(1024, d)] ~> Cell");

      for (int d = -5 ; d < 6 ; ++d) {
      __strict();
      __sreads("A ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");
      __xmodifies("&np[MINDEX1(1024, 4*bi+i+d)] ~> Cell");
        if (-3 > i+d && i+d < 6) {
          np[4*bi+i+d] = A[4*bi+i] + 1;
        }
        else {
          atomic_fetch_add(&ns[4*bi+i+d], 1);
        }
      }
      // __GHOST_BEGIN(focus, ro_matrix1_focus, "S, i");
      // D[MINDEX1(1024, i)] = S[MINDEX1(1024, i)];
      // __GHOST_END(focus);
    }
  }
}

void test_faa_1D_float (float* np, _Atomic float* ns, const float* A) {
  __reads("A ~> Matrix1(1024)");
  __modifies("np ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int bi = 0; bi < 256; bi++) {
    __strict();
    __sreads("1 ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");
    __xmodifies(
        "for i in 0..4 -> &np[MINDEX1(1024, bi * 4 + i)] ~> Cell");
    for (int i = 0; i < 4; ++i) {
      __strict();
      __sreads("A ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");
      __xmodifies("for d in 4*bi-2..4*bi+6 -> &np[MINDEX1(1024, d)] ~> Cell");

      for (int d = -5 ; d < 6 ; ++d) {
      __strict();
      __sreads("A ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");
      __xmodifies("&np[MINDEX1(1024, 4*bi+i+d)] ~> Cell");
        if (-3 > i+d && i+d < 6) {
          np[4*bi+i+d] = A[4*bi+i] + 1.25;
        }
        else {
          atomic_fetch_add(&ns[4*bi+i+d], 1.25);
        }
      }
      // __GHOST_BEGIN(focus, ro_matrix1_focus, "S, i");
      // D[MINDEX1(1024, i)] = S[MINDEX1(1024, i)];
      // __GHOST_END(focus);
    }
  }
} */
