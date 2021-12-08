typedef int T[3][2];

int get_1_x(int a[2][3]) { return a[0][1]; }

int get_1_y(T a) { return a[1][0]; }

int get_2_x(int a[2][3], int b[2][3]) { return (a[1][2] + b[1][2]); }

int get_2_y(T a, T b) { return (a[2][1] + b[2][1]); }

int main() {
  T t;
  int v[2][3];
  for (int i = 0; (i < 2); i++) {
    for (int j = 0; (j < 3); j++) {
      t[j][i] = ((2 * i) + j);
    }
  }
  int n =
      ((((t[2][1] + get_1_x(t)) + get_1_y(t)) + get_2_x(v, v)) + get_2_y(t, t));
  return 0;
}
