// This line should be included in the file in order for the output of "delocalize" to typecheck
int ANY(int maxValue){ return 0;}

const int N = 2;
typedef int T;
int main(){
  T a;
  for (int i = 0; i < 10; i++){
     a++;
  }
  int y = 0;
  return 0;
}


// typedef int T;
// const int N = 10;
// int my_core_id = 0;
// int f(int x){
//   return x*x;
// }
// int main(){
//   T a;
//   T x[N];
//   x[0] = a;
//   // int x[n];
//   for (int i = 0; i < N; i++){
//     a += x[i];
//     x[my_core_id]++;
//     a++;
//   }

//   int y = 0;
//   return 0;
// }