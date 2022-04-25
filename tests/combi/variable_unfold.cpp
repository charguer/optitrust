
void test_ref() {
    int a = 3;
    int& b = a;  // unfold/inline b
    int r = a + b;
}

void test_nonconst() {
    int a = 3; // basic.unfold/inline should succeed (but basic version should fail)
    int r = a + a;
}

void test_nonconst_fail() {
    int a = 3; // combi.unfold/inline should fail (because to_const fails)
    a = 3;
    int r = a + a;
}

const int CHUNK_SIZE = 10;
typedef struct {
  int x;
  int y;
  int z;
} vect;

typedef struct {
  vect pos;
  vect speed;
} particle;
typedef struct chunk {
  chunk *next;
  int size;
  particle items[CHUNK_SIZE];
} chunk;

typedef struct {
  chunk *front;
  chunk *back;
} bag;



int main() {

  bag *b = (bag*) malloc (100 * sizeof (bag));
  chunk* c = b-> front;

  int nb = (c ->size);
  for (int i = 0; i < nb; i++){
     particle* const p = &(c ->items[i]);

    vect f = {0,0,0};
    (p->pos) = f;
    (p->speed) = f;

    (p -> pos).x = 0;
    (p -> pos).y = 0;
    (p -> pos).z = 0;

    ((*p).speed).x = 0;
    ((*p).speed).y = 0;
    ((*p).speed).z = 0;
  }
  
  return 0;
}

