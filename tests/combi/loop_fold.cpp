int main(){
  double coef_x[3] = {0.,1.,2.};
  double sign_x[3] = {0.,1.,2.};
  double values[3];
  values[0] = (coef_x[0] + sign_x[0] * 1.) * (coef_x[0] + sign_x[0] * 2.) * (coef_x[0] + sign_x[0] * 3.);
  values[1] = (coef_x[1] + sign_x[1] * 1.) * (coef_x[1] + sign_x[1] * 2.) * (coef_x[1] + sign_x[1] * 3.);
  values[2] = (coef_x[2] + sign_x[2] * 1.) * (coef_x[2] + sign_x[2] * 2.) * (coef_x[2] + sign_x[2] * 3.);
}