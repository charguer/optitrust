int const N = 3;

int main() {
  int t[5] = { 0, 1, 2, 3, 4};
  for (int i = 0; i < 5; i++){
    t[i] = i + 3.14;
  }
  t[4] = 0;
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