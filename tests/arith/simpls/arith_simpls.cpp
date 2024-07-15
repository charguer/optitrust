
void rowSum(const int kn, const int* S, int* D, const int n, const int cn) {
  for (int ic = 0; ic < n * cn; ic++) {
    D[cn * (ic / cn) + ic % cn] =
        S[cn * (ic / cn) + ic % cn] +
        S[cn * (ic / cn + 1) + ic % cn];
  }
}
