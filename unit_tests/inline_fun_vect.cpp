
typedef struct { int x, y; } vect;

vect f(int a) { 
    vect r = { a, a };
    return r;
}

vect g (vect h) { 
    int x = 0;
    int y = 0;
    h.x = x;
    h.y = y;
    return h; 
}




int main() {
// first test 
    vect v = {1, 2};
    
    //int x;
    // x = g(v);
    // int res = ...
    //second test
    //int x;
    //x = g(v); 
    int x = 5;
    v = g(v);
    
    //v = f(3);
    // vect res =
    return 0;
}