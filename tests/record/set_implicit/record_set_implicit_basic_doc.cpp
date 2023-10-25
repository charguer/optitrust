
typedef struct {
  int x;
  int y;
} vect;

int main() {
  vect a;
  vect b;
  fuse:{
    a.x = b.x;
    a.y = b.y;
  }
}
