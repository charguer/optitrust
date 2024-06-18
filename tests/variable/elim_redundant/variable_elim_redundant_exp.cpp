void fn1() {
  int a = 4;
  for (int i = 0; i < 10; i++) {
    int c = a + a + i;
  }
  const int e = 5;
  int r = e + e + e;
}

void fn2() {
  double coef_x1[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
  double sign_x1[8] = {-1., -1., -1., -1., 1., 1., 1., 1.};
  double coef_y1[8] = {1., 1., 0., 0., 1., 1., 0., 0.};
  double sign_y1[8] = {-1., -1., 1., 1., -1., -1., 1., 1.};
  double coef_z1[8] = {1., 0., 1., 0., 1., 0., 1., 0.};
  double sign_z1[8] = {-1., 1., -1., 1., -1., 1., -1., 1.};
  int c1 = (int)(coef_x1[0] + coef_x1[1] + coef_x1[2] + coef_x1[3] +
                 coef_x1[4] + coef_x1[5] + coef_x1[6] + coef_x1[7]);
  double coef_x2[8] = {1., 1., 1., 1., 0., 0., 0., 0.};
  int c2 = (int)(coef_x2[0] + coef_x2[1] + coef_x2[2] + coef_x2[3] +
                 coef_x2[4] + coef_x2[5] + coef_x2[6] + coef_x2[7]);
}
