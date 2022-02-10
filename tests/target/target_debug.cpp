

typedef struct {
    int x;
    int y;
} vect;

int main1() {
  vect p = {0,0};
  vect b;
  group1: {
    b.x = p.x;
    b.y = p.y;
  }
  vect e;
  group2: {
    e.x = p.x;
    e.y = p.y;
  }
}





int main() {
  
  int x = 5;
  int t[2];
  x = 3;
  t[0] = 3;
  t[1] = 4;
  int a;
  a = t[0];
  return 0;
}

/*
typedef struct {
  int x;
  int y;
} vect;

typedef struct {
  int weight;
  vect pos;
  vect speed;
} obj;

vect f() { return {1, 1}; }

int main() {
  vect p = {0, 0};
  vect b; 
  b.x = (p.x);
  b.y = (p.y);
  vect u;
  obj a = {0, {0, 0}, {0, 0}};
  u.x = (a.pos.x);
  u.y = (a.pos.y);
  vect t[2];
  vect p2 = p;
  t[0].x = (p2.x);
  t[0].y = (p2.y);
  obj c;
  c.weight = (a.weight);
  c.pos.x = (a.pos).x;
  c.pos.y = (a.pos).y;
  c.speed.x = (a.speed).x;
  c.speed.y = (a.speed).y;
  return 0;
}*/