typedef struct {
  int x;
  int y;
  int z;
  int m;
} obj;


class OBJ{
  int x;

  public:
    void f(int y) {
      x = y + x;
    }
    void g(int y){
      x = y - x;
    }

};

int main() {
  obj a = {0, 1, 2, 3};
  int x;
  x = 5;
  return 0;
}
