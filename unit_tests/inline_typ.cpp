typedef unsigned int T_cint;

typedef const double T_uint;

typedef struct { int x; int y; } vect;

typedef vect T_vect; // typedef to be removed

typedef int[2][2] T_mat2x2;

typedef int*** T_mat3d; // typedef to be removed

int main() {
   T_cint x;
   T_unit y;
   T_vect v;
   T_mat2x2 m;
   T_mat3d M;
}

