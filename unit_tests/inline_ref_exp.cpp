
int main() {
   int x = 2;
   int r1 = x + x;
   x = 9;

   // inline reference on array cell
   int t[2] = { 4, 5 };
   int r2 = t[1] + t[1];
   t[1] = 9;

   // inline reference on matrix cell
   int m[3][3];
   int r3 = m[1][1] + m[1][1];
   m[1][1] = 9;

   // inline reference on matrix row
   int r4 = m[0][2];
   m[0][1] = 9;
}

