int f(int a) {
    return a * 3;
}

int g(int a) {
    return f(a) + 3;
}

int h(int a) {
    return f(a * 3) / f(a);
}

int i(int a) {
    int out = f(a);
    out *= g(a);
    return out;
}

int j(int a) {
    return f(g(a * i(a)));
}
