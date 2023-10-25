
typedef int*** T;

T t;

int main() {
  int i = 0;
  int j = 1;
  int k = 2;
  t[i][j][k] = 9; // should become t[j][i][k]
}

// TO INVESTIGATE LATER
// LATER: support swapping of t[i][j][k] into t[i][k][j] can perhaps be obtained by giving a specific type to t[i], but it's not obvious.
// LATER: support swapping of t[i][j][k] into t[k][i][j] or t[k][j][i] could be obtained in theory as a result of two successive swaps, but that remains to be demonstrated
