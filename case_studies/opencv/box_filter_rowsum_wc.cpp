void rowSum(const int kn, const T* S, ST* D, const int n, const int cn) {
  for (int c = 0; c < cn; c++) {
    for (int i = 0; i < n; i++) {
      D[MINDEX2(n, cn, i, c)] = reduce_spe1(i, i+kn, S, n+kn, cn, c);
    }
  }
}
