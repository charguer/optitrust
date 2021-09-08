int const N = 3;

int main() {
  {int x = 5;}
  return 0;
}

// int main() {
//   int s = 2;
// __TEMP_LABEL : {
//   {
//     int a = 1;
//     int b = (a + 2);
//     int c = 3;
//     int d = (c + 4);
//     int e = (d + 5);
//   }
//   {
//     int a = 1;
//     int b = (a + 2);
//     int c = 3;
//     int d = (c + 4);
//     int e = (d + 5);
//   }
//   {
//     int a = 1;
//     int b = (a + 2);
//     int c = 3;
//     int d = (c + 4);
//     int e = (d + 5);
//   }
// }
//   return 0;
// }