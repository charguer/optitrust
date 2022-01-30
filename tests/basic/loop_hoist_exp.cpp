int *t;

int *u;

int main() {
  int x_step[10];
  int z_step[10];
  for (int i = 0; i < 10; i++) {
    int &x = x_step[i];
    x = t[i];
    u[i] = x;
    int &z = z_step[i];
    z = x;
  }
}
