
typedef struct {
  int x;
  int y;
} vect;

int main() {
  vect v;
  v = (vect) {0,10};
}


/*

vect f(int a) {
  return { a-1, a };
}

int g(vect v) {
  return v.x + v.y;
}

int main() {
  int s = g(f(2));
  int p = f(3).x;
  vect v;
  v = (vect){0,10};

}
  int p = f(3).x;
*/