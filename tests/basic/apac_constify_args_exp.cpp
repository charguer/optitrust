typedef int * pINT;

using ppINT = int**;

struct A {};

void e(const int) {}

void f(const int) {}

void g(const float* const, const double&, const int* const* const&) {}

void h(const int* const, const int* const* const) {}

void i(const A&);

void i(const A&) {}