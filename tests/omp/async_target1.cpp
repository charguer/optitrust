float F(float);
const int N = 1000000000;
const int CHUNKSZ = 1000000;
void init(float *, int);
float Z[N];
void pipedF(){
   int C;
   init(Z, N);
   for (C=0; C<N; C+=CHUNKSZ){
      for (int i=0; i<CHUNKSZ; i++) Z[i] = F(Z[i]);
   }
}
