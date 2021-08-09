#include <omp.h>
#include <stdio.h>
int main( ) {
    int section_count = 0;
   
{
    {
        section_count++;
        /* may print the number one or two */
        printf( "section_count %d\n", section_count );
    }
    {
        section_count++;
        /* may print the number one or two */
        printf( "section_count %d\n", section_count );
    }
}
    return 0;
}

