int* t;
int* u;
int n;

int main() {
  for (int i = 1; i < n; i++) {
    {
       t[i] = i;
    }
    {
       u[i] += i;
    }
  }
}
