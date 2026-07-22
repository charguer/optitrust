// Difficulty: Level 4
// Request: target the write to field x

typedef struct {
  float x;
  float y;
} point;

void move(point* p) {
  p->x = p->x + 1.0f;
  p->y = p->y + 1.0f;
}
