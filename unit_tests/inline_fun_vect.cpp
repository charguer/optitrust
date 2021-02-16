
typedef struct { int x, y; } vect;

vect f(int a) { 
    vect r = { a, a };
    return r;
}
/*
int g (vect h) { 
    
    return h.x; 
}
*/

int main() {
// first test 
    vect v; // = {1, 2};
    //int x;
    // x = g(v);
    // int res = ...
    //second test
    v = f(3);
    // vect res =
    return 0;
}