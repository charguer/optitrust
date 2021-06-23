
typedef struct { int x, y; } vect;

vect f(int a) { 
    vect r = { a, a };
    return r;
}


int g (vect h) { 
    
    return h.x; 
}

/*
int h(vect a, vect b){
    int x = a.x + b.x;
    return x;
}
*/

int main() {
// first test 
    vect v = {1, 2};
    
    //int x;
    // x = g(v);
    // int res = ...
    //second test
    int x;
    x = g(v); 
    //int x = 5;
    
    //v = f(x);
    // vect res =
    return 0;
}