typedef struct {
  int x;
  int y;
  int z;
} vect;

int main(){

  vect a = {0,1,2};
  int x = 1;
  vect b = {3,4,5};
  vect c = {b.x + (vect){x * a.x, x * a.y, x * a.z}.x, b.y + (vect){x * a.x, x * a.y, x * a.z}.y, b.z + (vect){x * a.x, x * a.y, x * a.z}.z};
  return 0;
}
