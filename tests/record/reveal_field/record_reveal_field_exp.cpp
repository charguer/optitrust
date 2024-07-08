typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  int posX;
  int posY;
  int speedX;
  int speedY;
} particle;

typedef struct chunk {
  chunk* next;
  int size;
  int itemsWeight[2];
  int itemsPosX[2];
  int itemsPosY[2];
  int itemsSpeedX[2];
  int itemsSpeedY[2];
} chunk;

void test_arg(particle p) {
  particle q;
  q.posX = p.posX;
  q.posY = p.posY;
  q.speedX = p.speedX;
  q.speedY = p.speedY;
}

void test_chunk(chunk* c, particle p) {
  c->itemsPosX[0] = p.posX;
  c->itemsPosX[1] = p.posX;
  c->itemsPosY[0] = p.posY;
  c->itemsPosY[1] = p.posY;
}

int main() {
  vect p = {0, 0};
  vect s = {0, 0};
  particle a = {0, 0, 0, s.x, s.y};
  particle b = {0, p.x, p.y, s.x, s.y};
  particle c = {0, p.x, p.y, s.x, s.y};
  int nx = a.posX + a.speedX;
  int ny = a.posY + a.speedY;
  a.posX = 5;
  p.x = 5;
  vect t = {1, 0};
  int z = t.x;
  vect u = {0, 0};
}
