
int main() {
   // inline reference on an integer
   int x = 2;
   int& y = x;
   y = 3;
   int r1 = y + y;

   // inline reference on array cell
   int t[2] = { 4, 5 };
   int& a = t[1];
   a = 3;
   int r2 = a + a;

   // inline reference on matrix cell
   int m[3][3];
   int& b = m[1][1];
   b = 3;
   int r3 = b + b;

   // inline reference on matrix row
  //  const auto& v = m[0]; // LATER: what is the type of v?
  //  int r4 = v[2];

}

