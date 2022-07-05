
int f(int a) {
    int b = a;
    b = 1;
    return b;
}

void g(int a[2]) {
    a[f(1)]--;
}

void h(int &a) {
    a = 1;
}

void j(int &a) {
    h(a);
}

void k(int *a) {
    int * b = a;
    *b++;
}

void l1(int &a) {
    a += 1;
}

void l2(int &a) {
    l1(a);
}

void l3(int &a) {
    l2(a);
}

void m(int &a, int * b, int *& c) {
    int * d = a + b;
    d[0] = 1;
}