typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int weight;
    vect pos; // vect to be inlined here
    vect speed;
  } particle;



int main() {
  vect v1 = {0,0};
  vect v2 = {0,0};

  const particle p = {0, v1, v2};

  return 0;
  
}
