typedef struct {
  double z;
  double y;
  double x;
} vect;

const int NB_VECTS = 100;

typedef struct {
  vect speed;
  vect pos;
} particle;