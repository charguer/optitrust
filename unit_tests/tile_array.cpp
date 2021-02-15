
const int B = 8;

// Case of a variable sized array
typedef int* T;
T t;

// Case of a fixed sized array
// Below is the strange C syntax for saying that U is a shorthand for int[80]
typedef int U[80];
U u;

int main() {
   int i;
   int x = t[i];
   int y = u[i];
}


// LATER: can this be generalized to the case where B is not a const?
// i.e. making t go from int* to int**

// LATER: can we generalize this to the case where the size is not perfectly divisible by B? not clear.
