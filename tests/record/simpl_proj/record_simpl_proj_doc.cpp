
typedef struct {
  int x;
  int y;
} vect;

int main() {
  int a = (vect){1, 2}.x;
  int b = (vect){1, 2}.y;
}
