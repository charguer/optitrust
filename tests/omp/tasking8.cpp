int tp;
#pragma omp threadprivate(tp)
int var;
void work()
{
    {
        /* do work here */
        {
            tp++;
            /* do work here */
            {
                /* do work here but don't modify tp */
            }
            var = tp; //Value does not change after write above
        }
    }
}
