#include <optitrust.h>

// cas ou ça fonctionne
int main() {


int i = 24;
int k = i+3;
int j = 100 *3 +i;
int l = j+i;
}
// Pb avec le return
int main2() {

int i = 24;
int k = i+3;
int j = 100 *3 +i;
return 10;
}
// Pb avec le fait que k est utilisé après
int main3() {

int i = 24;
int k = i+3;
int j = 100 *3 +i;
j = k+i;
}
