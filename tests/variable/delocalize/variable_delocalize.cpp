// This line should be included in the file in order for the output of "delocalize" to typecheck

int ANY(int maxValue){ return 0;}

const int N = 2;
typedef int T;

int main(){
  T a;
  for (int i = 0; i < N; i++){
     a++;
  }
  int y = 0;
  for (int j = 0; j < N; j++){
     a++;
  }
  return 0;
}
