typedef int* pINT;

typedef int** ppINT;

struct A {};

void e(const int a) const {}

void f(const int a) const {}

void g(const float* const a, const double& b, const int* const* const& c) const {}

void h(const int* const a, const int* const* const b) const {}

void i(const A& a) const {}
