typedef struct {
  int x;
  int y;
} vect;

int main() {
  int t[2];
  t[0] = 4;
  t[1] = 5;

  vect u[2];
  u[0].x = 5;
  u[0].y = u[0].x + 6;
  u[1].x = 5;
  u[1].y = u[1].x + 6;

  vect v[2];
  vect a = {1,2};
  v[0] = a;
  v[1] = a;

  return 0;
}

void name_conflict() {
  int nc1[2];
  int nc2[2];
  nc1[0] = 1;
  nc1[1] = 2;
  nc2[0] = 3;
  nc2[1] = 4;
}
