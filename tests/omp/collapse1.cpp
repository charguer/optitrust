void bar(float *a, int i, int j, int k);

int kl, ku, ks, jl, ju, js, il, iu,is;

void sub(float *a)
{

    for (int k=kl; k<=ku; k+=ks)
       for (int j=jl; j<=ju; j+=js)
          for (int i=il; i<=iu; i+=is)
             bar(a,i,j,k);
}
