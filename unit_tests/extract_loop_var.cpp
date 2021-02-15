int* t;
int* u;

int main() {
  for (int i = 0; i < 10; i++) {
    double x; // variable to be extracted
    x = t[i];
    u[i] = x;
  }
}
