typedef int * pINT;

using ppINT = int**;

struct A {};

void e(const int) {}

void f(int) {}

void g(float *, double &, int const **&) {}

void h(pINT, ppINT) {}

void i(A &);

void i(A &) {}