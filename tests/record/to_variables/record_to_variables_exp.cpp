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
  int s_x;
  int s_y;
  s_x = p.x;
  s_y = p.y;
  int a_weight;
  vect a_pos;
  vect a_speed;
  a_weight = 0;
  a_pos = p;
  a_speed = s;
  return 0;
}
