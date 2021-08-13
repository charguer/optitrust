void foo ()
{
   int A[30], *p;
{
   p = &A[0];
   /* invalid because p[3] and A[3] are the same
    * location on the host but the array section
    * specified via p[...] is not a subset of A[0:4] */
   {
      A[2] = 0;
      p[8] = 0;
   }
}
}

