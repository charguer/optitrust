
int main() {
   // inline reference on an integer
   int x = 2;
   int& y = x;
   int r1 = y + y;
   y = 9;

   // inline reference on array cell
   int t[2] = { 4, 5 };
   int& a = t[1];
   int r2 = a + a;
   a = 9;

   // inline reference on matrix cell
   int m[3][3];
   int& b = m[1][1];
   int r3 = b + b;
   b = 9;

   // inline reference on matrix row
   auto& v = m[0]; // what is the type of v?
   int r4 = v[2];
   v[1] = 9;
}

