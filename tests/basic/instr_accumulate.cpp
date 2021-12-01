
typedef struct {
  int x;
  int y;

} vect;


int main () {

  int x = 10;
  test1: {
    x = x + 2;
    x = x + 3;
    x = x + 4;
    x = x + 5;
    x = x + 6;
    x += 7;
  }
  vect result = {0,0};
  test2: {
    result.x += 2;
    result.x += 3;
    result.x += 4;
    result.x += 5;
    result.x += 6;
    result.x += 7;
  }
  return 0;
}