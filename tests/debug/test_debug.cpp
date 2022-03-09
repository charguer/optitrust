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

  particle p1 = {0,{0,0},v1};
  particle p2 = {0, v1, v2};

  particle p3; 
  p3 = (particle) {0,{0,0},v1};
  particle p4;
  p4 = (particle){0, v1, v2};
  
}
