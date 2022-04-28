typedef struct {
    int x;
    int y; }
  vect;


int main() {
  vect p = {0,0};
  p.x = 5;
  p.y = 6;
  const int a = p.x;
  const int b = p.y;
  p.x = p.x + 7;
}
