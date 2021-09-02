
int main() {
   // inline pointer on an integer
   int x = 2;
   int* y = &x;
   int r1 = x + x;
   x = 9;

   // inline pointer on array cell
   int t[2] = { 4, 5 };
   int* a = &t[1];
   int r2 = t[1] + t[1];
   t[1] = 9;

   // inline pointer on matrix cell
   int m[3][3];
   int* b = &m[1][1];
   int r3 = m[1][1] + m[1][1];
   m[1][1] = 9;

   // inline pointer on matrix row
   const int* v = m[0];
   int r4 = m[0][2];
   m[0][1] = 9;
}

