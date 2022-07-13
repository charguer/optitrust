
typedef struct {
    int x;
    int y;
} vect;

int main() {
  vect p = {0,0};
  vect b;
  group1: {
    b.x = p.x;
    b.y = p.y;
  }
  vect e;
  group2: {
    e.x = p.x;
    e.y = p.y;
  }
}
