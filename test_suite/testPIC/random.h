#ifndef PIC_VERT_RANDOM
#define PIC_VERT_RANDOM

#include <stdio.h>        // function  fprintf (output strings on a stream)
                          // constant  stderr (standard error output stream)
#include <stdlib.h>       // function  exit (error handling)
                          // constant  EXIT_FAILURE (error handling)
#include <sys/time.h>     // function  gettimeofday
                          // type      struct timeval
#include <time.h>         // function  time
#ifndef _XOPEN_SOURCE
#    define _XOPEN_SOURCE // To enable gethostid.
#endif
#include <unistd.h>       // functions gethostid, getpid

typedef double (*next_random_double)(void);
typedef void (*seed_double_RNG_ULL)(unsigned long long int);
typedef void (*seed_double_RNG_UL )(unsigned long int);
typedef void (*seed_double_RNG_U  )(unsigned int);
typedef void (*seed_double_RNG_L  )(long int);
typedef void (*delete_double_RNG)(void);

/*
 * Hash function that takes 3 unsigned long ints.
 *
 * http://burtleburtle.net/bob/hash/doobs.html
 */
inline unsigned long int mix(unsigned long int a, unsigned long int b, unsigned long int c) {
    a=a-b;  a=a-c;  a=a^(c >> 13);
    b=b-c;  b=b-a;  b=b^(a << 8);
    c=c-a;  c=c-b;  c=c^(b >> 13);
    a=a-b;  a=a-c;  a=a^(c >> 12);
    b=b-c;  b=b-a;  b=b^(a << 16);
    c=c-a;  c=c-b;  c=c^(b >> 5);
    a=a-b;  a=a-c;  a=a^(c >> 3);
    b=b-c;  b=b-a;  b=b^(a << 10);
    c=c-a;  c=c-b;  c=c^(b >> 15);
    return c;
}

/*
 * Returns a 32-bit seed from time, process ID and host ID.
 * Also uses mpi_rank for the probable case where different MPI processes
 * are launched on the same machine.
 *
 * One could alternatively use bits from /dev/(u)random
 */
inline unsigned long int seed_32bits(int mpi_rank) {
    unsigned long int time_value = (unsigned long int)time((void*)0);
    unsigned long int process_id = (unsigned long int)getpid();
    unsigned long int host_id    = (unsigned long int)gethostid() + (unsigned long int)mpi_rank;
    return mix(time_value, process_id, host_id);
}

/*
 * Returns a 64-bit seed from time, process ID and host ID.
 * Also uses mpi_rank for the probable case where different MPI processes
 * are launched on the same machine.
 *
 * One could alternatively use bits from /dev/(u)random
 */
inline unsigned long long int seed_64bits(int mpi_rank) {
    struct timeval tv;
    gettimeofday(&tv, (void*)0);
    unsigned long int time_value1 = (unsigned long int)tv.tv_sec;
    unsigned long int time_value2 = (unsigned long int)tv.tv_usec;
    unsigned long int process_id  = (unsigned long int)getpid();
    unsigned long int host_id     = (unsigned long int)gethostid() + (unsigned long int)mpi_rank;
    unsigned long long int  low_32bits = (unsigned long long int)mix(time_value1, process_id, host_id);
    unsigned long long int high_32bits = (unsigned long long int)mix(time_value2, process_id, host_id);
    return low_32bits | (high_32bits << 32);
}

enum RNG {
    BAD_RAND_RNG,
    BAD_KISS_RNG,
    RAND_RNG,
    RAND48_RNG,
    KISS_RNG,
    MERSENNE_TWISTER_RNG,
    WELL_RNG,
    NB_RNG // Always has to be last if you update this enum !
};

char rng_names[NB_RNG][99];
next_random_double next_random_doubles[NB_RNG];
seed_double_RNG_ULL seed_double_RNG_ULLs[NB_RNG];
seed_double_RNG_UL  seed_double_RNG_ULs [NB_RNG];
seed_double_RNG_U   seed_double_RNG_Us  [NB_RNG];
seed_double_RNG_L   seed_double_RNG_Ls  [NB_RNG];
delete_double_RNG delete_double_RNGs[NB_RNG];


// Change the following line to change the RNG used in Pic-Vert
// (or compile with -DPICVERT_CHOSEN_RNG=...).
#if !defined(PICVERT_CHOSEN_RNG)
#    define PICVERT_CHOSEN_RNG WELL_RNG
#endif

#ifdef PICVERT_NB_RAND_CALLS
    unsigned long long int nb_rand_calls;
#endif

#define pic_vert_next_random_double next_random_doubles[PICVERT_CHOSEN_RNG]

// NB: too hard to translate, replace with void function
void pic_vert_seed_double_RNG (int seed);
/* #define pic_vert_seed_double_RNG(seed)                                             \ */
/*     seed_double_RNG_ULLs[PICVERT_CHOSEN_RNG]                                       \ */
/*         ? seed_double_RNG_ULLs[PICVERT_CHOSEN_RNG]((unsigned long long int)(seed)) \ */
/*         : (seed_double_RNG_ULs[PICVERT_CHOSEN_RNG]                                 \ */
/*             ? seed_double_RNG_ULs[PICVERT_CHOSEN_RNG]((unsigned long int)(seed))   \ */
/*             : (seed_double_RNG_Us[PICVERT_CHOSEN_RNG]                              \ */
/*                 ? seed_double_RNG_Us[PICVERT_CHOSEN_RNG]((unsigned int)(seed))     \ */
/*                 : seed_double_RNG_Ls[PICVERT_CHOSEN_RNG]((long int)(seed)))) */


void pic_vert_free_RNG () {
  delete_double_RNGs[PICVERT_CHOSEN_RNG];
}

#endif // ifndef PIC_VERT_RANDOM
