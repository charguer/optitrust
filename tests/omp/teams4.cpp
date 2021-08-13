const int N = 1024*1024;
float dotprod(float B[], float C[])
{
    float sum = 0.0;
    for (int i=0; i<N; i++)
        sum += B[i] * C[i];
    return sum;
}
