typedef struct {
  int x;
  int y;
} vect;

int main() {
  int t[2];
  t[0] = 4;
  t[1] = 5;
  vect ua;
  vect ub;
  ua.x = 5;
  ua.y = ua.x + 6;
  ub.x = 5;
  ub.y = ub.x + 6;
  vect va;
  vect vb;
  vect a = {1, 2};
  va = a;
  vb = a;
  return 0;
}

void name_conflict() {
  int nca;
  int ncb;
  int nca1;
  int ncb2;
  nca = 1;
  ncb = 2;
  nca1 = 3;
  ncb2 = 4;
}
