typedef int* pINT;

typedef int** ppINT;

struct A {};

void e(const int) const {}

void f(const int) const {}

void g(const float* const, const double&, const int* const* const&) const {}

void h(const int* const, const int* const* const) const {}

void i(const A&) const {}
