struct s { int label; union { int i; float f; };};

struct  {
  float weight;
  int pos_x;
  int pos_y;
  int speed_x;
  int speed_y;
} particles [3];


bool check_struct (struct s b)
{
    b.label  = 10;
    b.i = 10;
    b.f = 10;
    // etc.
}