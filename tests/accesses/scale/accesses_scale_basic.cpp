const int N = 5;
double t[N];

void f() {
   double t[3] = { 1., 2., 3. };
   double v;
   v = 2.0;
   const double u = 1.0;
   int i = 0;
   t[i] = t[i] + v * u;
}

