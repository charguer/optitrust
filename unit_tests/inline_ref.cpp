
int main() {
   // inline reference on an integer 
   int x = 2;
   int& y = x;
   int r1 = y + y;
   y = 9;

   // inline reference on array cell
   int[2] t = { 4, 5 };
   int& a = &t[1];
   int r2 = a + a;
   a = 9;
   
   // inline reference on matrix cell
   int[3][3] m;
   int& b = m[1][1];
   int r3 = b + b;
   b = 9;

   // inline reference on matrix row
   int& v = m[0];
   int r4 = v[2];
   v[1] = 9;
}

