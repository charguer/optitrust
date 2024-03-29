typedef struct {
  int rel_x;
  int rel_y;
} vect;

typedef struct {
  int weight;
  vect rel_pos;
  vect rel_speed;
} obj;

class TestFieldRename {
 private:
  int new_x;

 public:
  int new_f(int y) { return y + x; }
  int new_g(int y) { return this->x + y; }
};

vect f() { return (vect){1, 1}; }

void test_struct_field_rename() {
  vect p = {0, 0};
  vect b;
  b.rel_x = p.rel_x;
  b.rel_y = p.rel_y;
  vect u;
  obj a = {0, {0, 0}, {0, 0}};
  u.rel_x = a.rel_pos.rel_x;
  u.rel_y = a.rel_pos.rel_y;
  vect t[2];
  vect p2 = p;
  t[0].rel_x = p2.rel_x;
  t[0].rel_y = p2.rel_y;
  obj c;
  c.weight = a.weight;
  c.rel_pos.rel_x = a.rel_pos.rel_x;
  c.rel_pos.rel_y = a.rel_pos.rel_y;
  c.rel_speed.rel_x = a.rel_speed.rel_x;
  c.rel_speed.rel_y = a.rel_speed.rel_y;
}

void test_class_members_rename() {
  TestFieldRename t;
  int a = t.new_f(10);
  int b = t.new_g(a);
}

int main() {}
