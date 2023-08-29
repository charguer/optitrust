typedef int * pINT;

using ppINT = int**;

struct A {};

void e(const int a) {}

void f(int a) {}

// TODO: Be able to use anonymous arguments, e.g. void g(float *, double &, ...)
void g(float * a, double & b, int const **& c) {}

void h(pINT a, ppINT b) {}

void i(A & a) {}
