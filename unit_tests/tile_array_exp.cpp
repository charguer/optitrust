const int B = 8;

// case of a variable sized array
// case of a fixed sized array

typedef int X[B];

typedef X *T;

T t;





// case of a variable sized array
typedef (int[B]) T;
T* t;

// case of a fixed sized array
int u[10][B];

int main() {
   int i;
   int x = t[i/B][i%B];
   int y = u[i/B][i%B];
}

