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

void depart(_Atomic int* ns, int* np, int* S) {
  __modifies("np ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i = 0; i < 1024; ++i) {
    __strict();
    __smodifies("np ~> Matrix1(1024)");
    __sreads("S ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");
    for (int d = -5 ; d < 6 ; ++d) {
      __strict();
      __smodifies("np ~> Matrix1(1024)");
      __sreads("S ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");

      __GHOST_BEGIN(focuss, ro_matrix1_focus, "S, i");
      int chaleur_case_i = S[MINDEX1(1024, i)];
      __GHOST_END(focuss);

      const int j = i+d;
      if (0 <= j && j < 1024) {
        __ghost(assume, "in_range(j, 0..1024)");
        __GHOST_BEGIN(focusnp, matrix1_focus, "np, j");
        np[MINDEX1(1024, j)] += chaleur_case_i; // add factor ?
        __GHOST_END(focusnp);
      }
    }
  }
}

/* void matrix_copy(_Atomic int* ns, int* np, int* S) {
  __modifies("np ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i = 0; i < 1024; ++i) {
    __strict();
    __xmodifies("&np[MINDEX1(1024, i)] ~> Cell");
    __sreads("S ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");
    for (int d = -5 ; d < 6 ; ++d) {
    __strict();
    __smodifies("&np[MINDEX1(1024, i)] ~> Cell");
    __sreads("S ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");

      if (d==0) {
        __GHOST_BEGIN(focus2, ro_matrix1_focus, "S, i");
        np[MINDEX1(1024, i)] += S[MINDEX1(1024, i)];
        __GHOST_END(focus2);
      }
      else {
        const int j = i+d;
        if (0 <= j && j < 1024) {

          __ghost(assume, "in_range(j, 0..1024)"); // LATER: deduced from if is_in_range
          __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, j");
          __GHOST_BEGIN(focus, ro_matrix1_focus, "S, j");
          // atomic_set_int(&ns[MINDEX1(1024, i)], S[MINDEX1(1024, i)]);
          int a = atomic_fetch_add(&ns[MINDEX1(1024, j)], S[MINDEX1(1024, j)]);
          __GHOST_END(focus);
          __GHOST_END(focusns);
        }
      }
    }
  }
}
 */

/* void matrix_copy_tiled(_Atomic int* ns, int* np, int* S) {
  __modifies("np ~> Matrix1(1024)");
  __reads("S ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");
  __ghost(tile_divides,
          "items := fun (i: int) -> "
          "&np[MINDEX1(1024, i)] ~> Cell");
  for (int bi = 0; bi < 256; bi++) {
    __strict();
    __requires("_Fraction");
    __sreads("S ~> Matrix1(1024)");
    __xmodifies("for i in 0..4 -> &np[MINDEX1(1024, bi * 4 + i)] ~> Cell");

    for (int i = 0; i < 4; ++i) {
      __strict();
      __xmodifies("&np[MINDEX1(1024, bi * 4 + i)] ~> Cell");
      __sreads("S ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");
      __ghost(tiled_index_in_range,
              "tile_index := bi, index := i, div_check := tile_div_check_i");
      for (int d = -5 ; d < 6 ; ++d) {
      __strict();
      __smodifies("&np[MINDEX1(1024, bi * 4 + i)] ~> Cell");
      __sreads("S ~> Matrix1(1024)");
      __satomic("ns ~> Matrix1(1024)");

        if (d==0) {
          __GHOST_BEGIN(focus2, ro_matrix1_focus, "S, bi * 4 + i");
          np[MINDEX1(1024, bi * 4 + i)] += S[MINDEX1(1024, bi * 4 + i)];
          __GHOST_END(focus2);
        }
        else {
          const int j = bi * 4 + i + d;
          if (0 <= j && j < 1024) {

            __ghost(assume, "in_range(j, 0..1024)"); // LATER: deduced from if is_in_range
            __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, j");
            __GHOST_BEGIN(focus, ro_matrix1_focus, "S, j");
            // atomic_set_int(&ns[MINDEX1(1024, i)], S[MINDEX1(1024, i)]);
            int a = atomic_fetch_add(&ns[MINDEX1(1024, j)], S[MINDEX1(1024, j)]);
            __GHOST_END(focus);
            __GHOST_END(focusns);
          }
        }
      }
    }
  }
    __ghost(untile_divides,
          "div_check := tile_div_check_i, items := fun (i: int) -> "
          "&np[MINDEX1(1024, i)] ~> Cell");

} */



/* void faa_with_for_and_if (_Atomic int* ns, const int* A) {
  __reads("A ~> Matrix1(1024)");
  __atomic("ns ~> Matrix1(1024)");

  for (int i=0 ; i<1024 ; i++) {
    __strict();
    __sreads("A ~> Matrix1(1024)");
    __satomic("ns ~> Matrix1(1024)");

    for (int d = -5 ; d < 6 ; ++d) {
      const int j = i+d;
      if (0 <= j && j < 1024) {
        __ghost(assume, "in_range(j, 0..1024)"); // LATER: deduced from if is_in_range

        __GHOST_BEGIN(focusns, atomic_matrix1_focus, "ns, j");
        __GHOST_BEGIN(focusA, ro_matrix1_focus, "A, j");
          int a = atomic_fetch_add(&ns[MINDEX1(1024, j)], A[MINDEX1(1024, j)]);
        __GHOST_END(focusA);
        __GHOST_END(focusns);
      }
    }
  }
} */


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

/*
  - Changer le type des opérations atomiques pour que le deuxième argument n'ait pas à l'être : quel fichier ?
  - Les sharedatomic disparaissent dans l'output (même dans l'input ?). Pas les atomic sur les fonctions.
    C'est dans quel fichier déjà ?
  - Définir une fonction atomic_set (optitrust.h ou dans le typeur, comme binopset)
    de type atomic T* -> T -> unit, pour pouvoir faire un A[i] = k quand k n'est pas de type atomic.


  n ~> Matrix

  NON) delocalize (transformer un tableau en deux): technique à mettre en place -> on fait pas.
  Code existant, problème est de gérer les permissions. ça rajoute une dimension, mais
  ce qui est technique c'est le array_to_variables qui sépare les deux pour donner des
  permissions différentes. n[i] = n[0,i] + n[1,i], mais là, il faut t[0,i] -> t0[i].
  Les t~> Matrix2(2,N) deviennent t0 ~> Matrix1(N), t1 ~> Matrix1(N)
  La tranfo génère aussi le reduce de fin, qui reconstruit n en sommant n0 et n1.
  On se permettra de faire des substitutions et d'accumuler des choses dans l'un ou l'autre
  sans distinction ; on considèrera qu'on a les deux tableaux en entrée déjà.

  1) Introduction du if
     voir if_insert.ml.
     !! If_basic.insert ~cond:(expr "qqc") [cFor "d" ; cIf ""; dThen];
     Il faudra construire à la place de expr, une conndition explicitement avec des termes :
     term_and (trm_equal ...).
     Pour les trm_var, l'argument se crée avec find_var (voir nbx exemples, dans tel bloc chercher telle variable).

     Insert_if fait déjà la validation (L24 dans if_basic : vérifie que c'est pure).
     Condition +- 1/2 bloc du bord du bloc.

  2) Dans le else : on remplace np par ns.
    Il faudra changer les ghosts (atomic et non shared qqc). Ne change pas le comportement du code.
    Il faut faire les deux en même temps, sans retyper entre les deux :
    Trace.without resource computation between steps (fun () -> tranfo 1; transfo2.
    a) replace ns par np (expr_basic.replace cVar np en target
       ne pas oublier term_var (find_var ns) ici aussi)) (ne sera pas validé.)
    b) Puis changer les ghosts : delete sur les focus et refaire des nouvelles. Sinon par index sur l'instruction.
       A la place du matrix focus, on met atomic_matrix_focus. Et ça doit typer.
    c) Remplacer += par faa. (pas validé, mais peut l'être.)

  3) Changer les contrats des boucles et ajouter un ghost (set_contract.ml)
      - Certains smodifies sur np doivent devenir des xmodifies (sur les cases voisines de i <1/2 bloc)
      - Ajouter une ghost qui dit que posséder touts les cases i, c'est pareil que de posséder, pour une couleur donnée,
      pour chaque bloc de cette couleur, le bloc et ses 1/2 voisins.
      __consumes for i in 0..N -> &t[i] ~> Cell
      __produces for bi in range (ci, N/4, 2) -> for i from 1/2b d'avant to d'après) -> t[i] ~> Cell
      Avant la boucle sur bi, quand on connait ci.
      for ci
        smodifies(np ~> Matrix(...))
        ghost(c'est pareil les blocs)
        for bi (à paralléliser)
          __xmodifies(np[cases proches])
          for i
            __smodifies
            for d
              __smodifies
              j =
              focus j







  toatomic (cf toconst): atomic type, atomic contract, atomic operation => avant dernier
  all at the same time, or not if the standard operations can be performed on atomic types.

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
