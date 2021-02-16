typedef struct {
  int y;
  int x;
} vect;

int main() {
  vect v;
  {
    int a = 3;
    vect res;
    vect r = {a, a};
    res = r;
    overloaded = (v, res);
  }
  return 0;
}
