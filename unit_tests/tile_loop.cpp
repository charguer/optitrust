
int N;
int* t;

int BLOCK; // assumes BLOCK DIVIDES N

int main() {
  for (int i = 0; i < N; i++) {
    int i1 = i / BLOCK;
    int i2 = i % BLOCK;
    t[i] = i1 + i2;
  }
}
