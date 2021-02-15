int* t,u;
int n;

int main() {
  for (int i = 1; i < n; i++) {
     t[i] = i;
  }
  for (int i = 1; i < n; i++) {
     u[i] += i;
  }
}

// note: the transformation introduces labels, let's make the script clean all labels

