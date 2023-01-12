const int N = 10000;
float Q[N][N];
float Pfun(const int i, const int k)
{ return Q[i][k] * Q[k][i]; }
float accum(int k)
{
    float tmp = 0.0;
    for(int i=0; i < N; i++)
        tmp += Pfun(i,k);
    return tmp;
}
