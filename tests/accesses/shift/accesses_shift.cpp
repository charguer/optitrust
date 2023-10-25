const int N = 5;
double t[N];

void test_var() {
  double x = 0.0;
  x = 1.0;
  double y = x * 1.0;
  x = x * 2.0;
}

void test_array() {
  double t[2] = { 1., 2 };
  t[0] = t[0] * 1.0;
}


int main() {
   double t[3] = { 1., 2., 3. };
   double v = 2.0;
   int i = 0;
   t[i] = t[i] * v;
   return 0;
}

