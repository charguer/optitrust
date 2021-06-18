
/* disallowed: typedef struct foo { int x; int y; } vect; */

typedef struct { int x; int y; } vect;

typedef vect myvect;

int main() {
    vect v = { 1, 2 };
    myvect w = { 3, 4 };
    int vx = v.x;
    int wy = w.y;
}
