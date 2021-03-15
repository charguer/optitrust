
typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

int main() {
  vect p = {0,0};
  vect b ;
  b = p;
  obj a = {0,{0,0},0};

  a.pos = p;
}

