
typedef int T;

T CHOOSE (int nb, T a, T b) {return a;}

int main() {
  T a = 0;
  T b = 1;
  T c;
  c = CHOOSE(2, a, b);
  return 0;
}
