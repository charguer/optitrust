typedef struct {
  int x;
  int y;
} vect;

vect f(int a) { return {(a - 1), a}; }

int g(vect v) { return (v.x + v.y); }

int main() {
  vect r = {(2 - 1), 2};
  int s = g(r);
  vect r = {(3 - 1), 3};
  int p = r.x;
}
