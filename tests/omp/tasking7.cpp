int tp;
#pragma omp threadprivate(tp)
int var;
void work()
{
    {
        /* do work here */
        {
            tp = 1;
            /* do work here */
            {
                /* no modification of tp */
            }
            var = tp; //value of tp can be 1 or 2
        }
        tp = 2;
    }
}
