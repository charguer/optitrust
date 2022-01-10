int main() {
  int a = 0;
iterations : {
  a += 0;
  a += 1;
  a += 2;
  a += 3;
}
}

int more() {
  double coef_x[3] = {0., 1., 2.};
  double sign_x[3] = {0., 1., 2.};
  double values[3];
  for (int k = 0; (k < 3); k++) {
    values[k] =
        (((coef_x[k] + (sign_x[k] * 1.)) * (coef_x[k] + (sign_x[k] * 2.))) *
         (coef_x[k] + (sign_x[k] * 3.)));
  }
}
