void rowSum(const int kn, const int* S, int* D, const int n, const int cn) {
  for (int ic = 0; ic < n * cn; ic += 1) {
    D[ic] = S[ic] + S[ic + cn];
  }
}
