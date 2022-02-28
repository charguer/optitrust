typedef struct {

  double x;
  double y;
  double z;
} vect;

typedef struct {
  double v[8];
} double_nbCorners;

typedef struct {
  vect v[8];
} vect_nbCorners;


vect matrix_vect_mul(const double_nbCorners coeffs,
                     const vect_nbCorners matrix) {
  vect res = {0., 0., 0.};
  res.x = res.x + coeffs.v[0] * (matrix.v[0].x);
  res.x = res.x + coeffs.v[1] * (matrix.v[1].x);
  res.x = res.x + coeffs.v[2] * (matrix.v[2].x);
  res.x = res.x + coeffs.v[3] * (matrix.v[3].x);
  res.x = res.x + coeffs.v[4] * (matrix.v[4].x);
  res.x = res.x + coeffs.v[5] * (matrix.v[5].x);
  res.x = res.x + coeffs.v[6] * (matrix.v[6].x);
  res.x = res.x + coeffs.v[7] * (matrix.v[7].x);
  res.y = res.y + coeffs.v[0] * (matrix.v[0].y);
  res.y = res.y + coeffs.v[1] * (matrix.v[1].y);
  res.y = res.y + coeffs.v[2] * (matrix.v[2].y);
  res.y = res.y + coeffs.v[3] * (matrix.v[3].y);
  res.y = res.y + coeffs.v[4] * (matrix.v[4].y);
  res.y = res.y + coeffs.v[5] * (matrix.v[5].y);
  res.y = res.y + coeffs.v[6] * (matrix.v[6].y);
  res.y = res.y + coeffs.v[7] * (matrix.v[7].y);
  res.z = res.z + coeffs.v[0] * (matrix.v[0].z);
  res.z = res.z + coeffs.v[1] * (matrix.v[1].z);
  res.z = res.z + coeffs.v[2] * (matrix.v[2].z);
  res.z = res.z + coeffs.v[3] * (matrix.v[3].z);
  res.z = res.z + coeffs.v[4] * (matrix.v[4].z);
  res.z = res.z + coeffs.v[5] * (matrix.v[5].z);
  res.z = res.z + coeffs.v[6] * (matrix.v[6].z);
  res.z = res.z + coeffs.v[7] * (matrix.v[7].z);
  return res;
}
