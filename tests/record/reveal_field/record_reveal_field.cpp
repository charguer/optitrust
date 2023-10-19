typedef struct {
    int x;
    int y; }
  vect;

typedef struct {
    int weight;
    vect pos; // vect to be inlined here
    vect speed;
  } particle;


typedef struct chunk {
   struct chunk *next;
   int size;
   particle items[2];
} chunk;



void test_arg(particle p){
 
 particle q;
 q.pos.x = p.pos.x;
 q.pos.y = p.pos.y;
 q.speed.x = p.speed.x;
 q.speed.y = p.speed.y;
}


void test_chunk (chunk* c, particle p) {

  c->items[0].pos.x = p.pos.x;
  c->items[1].pos.x = p.pos.x;

  c->items[0].pos.y = p.pos.y;
  c->items[1].pos.y = p.pos.y;

}

int main() {
  vect p = {0,0};
  vect s = {0,0};

  particle a = {0,{0,0},s};
  particle b = {0, p, s};
  particle c = {0, {p.x, p.y}, s};
  int nx = a.pos.x + a.speed.x;
  int ny = a.pos.y + a.speed.y;

  a.pos.x = 5;
  p.x = 5; // no change

  vect t = {1,0};
  int z = t.x;
  vect u = {0,0};


}
