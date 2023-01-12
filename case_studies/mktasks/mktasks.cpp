
int f(int i, int j) {
    return 0;
}

int g(int a) {return a;}

int main()
{
    int b;
    if (true) {
        int a;
        f(1, 3);
        f(a, f(a, 1));
    }
    else {
        int a;
        f(1, 3);
        f(a, f(a, 1));
    }
    {
        int a;
        f(1, g(a));
        f(a, f(a, 1));
        int c;
    }
    int d = g(g(1));
    return 0;
}