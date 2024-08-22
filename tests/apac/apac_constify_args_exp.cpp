typedef int* pINT;

typedef int** ppINT;

struct A {};

void e(const int a) const {}

void f(int a) const {}

void g(const float* a, const double& b, const int* const* const& c) const {}

void h(const int* a, const int* const* b) const {}

void i(const A& a) const {}
