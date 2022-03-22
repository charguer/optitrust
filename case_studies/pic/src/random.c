#include <stdio.h>           // function  fprintf (output strings on a stream)
                             // constant  stderr (standard error output stream)
#ifndef _XOPEN_SOURCE
#  define _XOPEN_SOURCE      // To enable drand48 and friends.
#endif
#include <stdlib.h>          // functions exit (error handling)
                             //           rand,  drand48 (random generators)
                             //           srand, srand48 (random generator seeders)
                             // constants EXIT_FAILURE (error handling)
                             //           RAND_MAX
                             // type      size_t
// If you have the GNU Scientific Library, you can compile Pic-Vert with -lgsl -lgslcblas
// and with -DPICVERT_GSL, allowing you to use the Mersenne Twister RNG.
#ifdef PICVERT_GSL
#    include <gsl/gsl_rng.h> // functions gsl_rng_alloc, gsl_rng_set, gsl_rng_uniform, gsl_rng_free
                             // types     gsl_rng, gsl_rng_type
                             // constant  gsl_rng_mt19937
#endif
#include "random.h"          // types     next_random_double, seed_double_RNG_UL, seed_double_RNG_U,
                             //           seed_double_RNG_L, delete_double_RNG
                             // constants BAD_RAND_RNG, RAND_RNG, RAND48_RNG, MERSENNE_TWISTER_RNG, WELL_RNG, NB_RNG

/************
 * rand RNG *
 ************/
/*
 * Uses only one rand() call to provide a double number.
 *
 * WARNING: Low RAND_MAX values (< 2**64 - 1) are not sufficient to output all
 * possible doubles, so for portability: DO NOT USE rand() like this ! (-:
 */
double bad_rand_next_random_double(void) {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
    return ((double)rand()) / ((double)RAND_MAX + 1.);
}

#define MASK_53_BITS 9007199254740991
#define TWO_POWER_53 9007199254740992.0
#define TWO_POWER_27        134217728.0
#define TWO_POWER_18           262144.0
/*
 * Combines different rand() calls to provide a double number with all 53
 * random bits.
 *
 * RAND_MAX is guaranteed to be at least 32 767 (2**15 - 1).
 * For this value, we can combine 4 rand() values, as suggested in:
 * https://stackoverflow.com/questions/2704521/generate-random-double-numbers-in-c
 *
 * Here we handle any RAND_MAX value to be more portable, with different cases
 * handled in a similar fashion.
 *
 * N.B.: For performance issues, one could move the ifs at the exterior of the
 * function and use preprocessor #ifs instead (leading to less readable code).
 */
double rand_next_random_double() {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
    unsigned long long rand_max_plus_one = (unsigned long long)RAND_MAX + 1ULL;
    if (RAND_MAX < TWO_POWER_18)
        return (double)((
                 (unsigned long long)rand() +
                ((unsigned long long)rand() * rand_max_plus_one) +
                ((unsigned long long)rand() * rand_max_plus_one * rand_max_plus_one) +
                ((unsigned long long)rand() * rand_max_plus_one * rand_max_plus_one * rand_max_plus_one)
            ) & MASK_53_BITS) / TWO_POWER_53;
    else if (RAND_MAX < TWO_POWER_27)
        return (double)((
                 (unsigned long long)rand() +
                ((unsigned long long)rand() * rand_max_plus_one) +
                ((unsigned long long)rand() * rand_max_plus_one * rand_max_plus_one)
            ) & MASK_53_BITS) / TWO_POWER_53;
    else if (RAND_MAX < TWO_POWER_53)
        return (double)((
                 (unsigned long long)rand() +
                ((unsigned long long)rand() * rand_max_plus_one)
            ) & MASK_53_BITS) / TWO_POWER_53;
    else
        return (double)((
                 (unsigned long long)rand()
            ) & MASK_53_BITS) / TWO_POWER_53;
}
void rand_seed_double_RNG(unsigned int seed) {
    srand(seed);
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls = 0;
#endif
}


/**************
 * rand48 RNG *
 **************/
double rand48_next_random_double(void) {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
    return drand48();
}
void rand48_seed_double_RNG(long int seed) {
    srand48(seed);
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls = 0;
#endif
}


/***********************************************************************************************
 * KISS (Keep It Simple Stupid) RNG by G. Marsaglia and modified by D.T. Jones                 *
 * "Good Practice in (Pseudo) Random Number Generation for Bioinformatics Applications" (2010) *
 * http://www0.cs.ucl.ac.uk/staff/D.Jones/GoodPracticeRNG.pdf                                  *
 ***********************************************************************************************/
static unsigned int kiss_x, kiss_y, kiss_z, kiss_c; // Seed variables
unsigned int KISS() {
    unsigned long long t;
    kiss_x = 314527869 * kiss_x + 1234567;
    kiss_y ^= kiss_y << 5;
    kiss_y ^= kiss_y >> 7;
    kiss_y ^= kiss_y << 22;
    t = 4294584393ULL * kiss_z + kiss_c;
    kiss_c = t >> 32;
    kiss_z = t;
    return kiss_x + kiss_y + kiss_z;
}

#define TWO_POWER_32 4294967296.0
/*
 * Uses only one KISS() call to provide a double number.
 *
 * WARNING: The maximum output value of KISS (2^32 - 1) is not sufficient to
 * output all possible doubles, so DO NOT USE JKII() like this ! (-:
 */
double bad_KISS_next_random_double(void) {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
    return ((double)KISS()) / ((double)TWO_POWER_32);
}

/*
 * In this function, the magic numbers are:
 *               134 217 728 = 2**27.
 *     9 007 199 254 740 992 = 2**53.
 *
 * This function uses the fact that KISS returns values in {0, 1, ... 2**32 - 1}
 */
double KISS_next_random_double(void) {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
    unsigned int a, b;
    a = KISS() >> 6; // Upper 26 bits
    b = KISS() >> 5; // Upper 27 bits
    return ((double)a * 134217728.0 + (double)b) / 9007199254740992.0;
}
void KISS_seed_double_RNG(unsigned long long int seed) {
    kiss_x = 123456789;
    kiss_y = 987654321;
    kiss_z = (unsigned int)(seed      ); // Lower  32 bits // 43219876;
    kiss_c = (unsigned int)(seed >> 32); // Higher 32 bits // 6543217;
    if (kiss_c == 0) kiss_c = 6543217;   // If the seed is only 32 bits
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls = 0;
#endif
}


/*********************************************************************************************************
 * Mersenne Twister RNG by M. Matsumoto and T. Nishimura                                                 *
 * "Mersenne Twister: A 623-dimensionally Equidistributed Uniform Pseudo-random Number Generator" (1998) *
 * http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/emt.html                                              *
 * Code provided in the GNU Scientific Library                                                           *
 * https://www.gnu.org/software/gsl/manual/html_node/Random-Number-Generation.html                       *
 *********************************************************************************************************/
#if defined(__GSL_RNG_H__)
gsl_rng* picvert_gsl_rng;
#endif
double gsl_next_random_double(void) {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
#if defined(__GSL_RNG_H__)
    return gsl_rng_uniform(picvert_gsl_rng);
#else
    fprintf(stderr, "Pic-Vert was not linked with the GNU Scientific Library. See %s.\n", __FILE__);
    exit(EXIT_FAILURE);
#endif
}
void gsl_seed_double_RNG(unsigned long int seed) {
#if defined(__GSL_RNG_H__)
    picvert_gsl_rng = gsl_rng_alloc(gsl_rng_mt19937);
    gsl_rng_set(picvert_gsl_rng, seed);
#else
    fprintf(stderr, "Pic-Vert was not linked with the GNU Scientific Library. See %s.\n", __FILE__);
    exit(EXIT_FAILURE);
#endif
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls = 0;
#endif
}
void gsl_delete_double_RNG(void) {
#if defined(__GSL_RNG_H__)
    gsl_rng_free(picvert_gsl_rng);
#else
    fprintf(stderr, "Pic-Vert was not linked with the GNU Scientific Library. See %s.\n", __FILE__);
    exit(EXIT_FAILURE);
#endif
}


/***************************************************************************************************
 * WELL (Well Equidistributed Long-period Linear) RNG by F. Panneton, P. L'Ecuyer and %. Matsumoto *
 * "Improved Long-period Generators Based on Linear Recurrences Modulo 2" (2006)                   *
 * http://www.iro.umontreal.ca/~panneton/WELLRNG.html                                              *
 ***************************************************************************************************/
void InitWELLRNG19937a(unsigned int *);
double (*WELLRNG19937a)(void);
double WELL_next_random_double(void) {
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls++;
#endif
    return (*WELLRNG19937a)();
}
#define LENGTH_SEED_ARRAY 624 // Cf. R and InitWELLRNG19937a later in this file.
void WELL_seed_double_RNG(unsigned long long int seed) {
    // Initialization adapted from dSFMT 2.2.3 (Saito, M. and Matsumoto, M.)
    // Cf. function "dsfmt_chk_init_gen_rand" in dSFMT.c
    // http://www.math.sci.hiroshima-u.ac.jp/~m-mat/MT/SFMT/dSFMT-src-2.2.3.tar.gz
    unsigned int seed_array[LENGTH_SEED_ARRAY];
    seed_array[0] = (unsigned int)(seed      ); // Lower  32 bits
    seed_array[1] = (unsigned int)(seed >> 32); // Higher 32 bits
    size_t index_begin = seed_array[1] == 0 ? 1 : 2; // If the seed is only 32 bits
    for (size_t i = index_begin; i < LENGTH_SEED_ARRAY; i++) {
        seed = 1812433253ULL * (seed ^ (seed >> 30)) + i;
        seed_array[i] = (unsigned int)(seed);
    }
    InitWELLRNG19937a(seed_array);
#ifdef PICVERT_NB_RAND_CALLS
    nb_rand_calls = 0;
#endif
}


/*
 * How to update those arrays with a new RNG:
 * 0. Update the RNG enum with a new one in random.h
 * 1. Create the needed functions (next_double, seeder, deleter if needed) in
 *    this file, and put a pointer to them in the following arrays.
 * 2. If the seed is unsigned long int, put the seeder function in seed_double_RNG_UL
 *    If the seed is unsigned      int, put the seeder function in seed_double_RNG_U
 *    If the seed is          long int, put the seeder function in seed_double_RNG_L
 *    Put the null pointer "(void*)0" in the two other arrays.
 *    (if the seed has another type:
 *       (a) create a 4th, 5th... array
 *       (b) modify the pic_vert_seed_double_RNG macro inside random.h)
 * 3. Change PICVERT_CHOSEN_RNG in random.h to use your new RNG.
 * 4. Profit ! Nothing else has to be changed, the simulations will run with
 *    the new RNG without changing any other line.
 */
char rng_names[NB_RNG][99] = {
    [BAD_RAND_RNG]         = "rand RNG (C default), bad usage",
    [BAD_KISS_RNG]         = "KISS RNG (Keep It Simple Stupid), bad usage",
    [RAND_RNG]             = "rand RNG (C default), multiple calls combined",
    [RAND48_RNG]           = "rand48 RNG (POSIX default)",
    [KISS_RNG]             = "KISS RNG (Keep It Simple Stupid)",
    [MERSENNE_TWISTER_RNG] = "Mersenne Twister RNG (GSL default)",
    [WELL_RNG]             = "WELL RNG (Well Equidistributed Long-period Linear)",
};
next_random_double next_random_doubles[NB_RNG] = {
    [BAD_RAND_RNG]         = &bad_rand_next_random_double,
    [BAD_KISS_RNG]         = &bad_KISS_next_random_double,
    [RAND_RNG]             = &rand_next_random_double,
    [RAND48_RNG]           = &rand48_next_random_double,
    [KISS_RNG]             = &KISS_next_random_double,
    [MERSENNE_TWISTER_RNG] = &gsl_next_random_double,
    [WELL_RNG]             = &WELL_next_random_double,
};
seed_double_RNG_ULL seed_double_RNG_ULLs[NB_RNG] = {
    [BAD_RAND_RNG]         = (void*)0,
    [BAD_KISS_RNG]         = &KISS_seed_double_RNG,
    [RAND_RNG]             = (void*)0,
    [RAND48_RNG]           = (void*)0,
    [KISS_RNG]             = &KISS_seed_double_RNG,
    [MERSENNE_TWISTER_RNG] = (void*)0,
    [WELL_RNG]             = &WELL_seed_double_RNG,
};
seed_double_RNG_UL  seed_double_RNG_ULs [NB_RNG] = {
    [BAD_RAND_RNG]         = (void*)0,
    [BAD_KISS_RNG]         = (void*)0,
    [RAND_RNG]             = (void*)0,
    [RAND48_RNG]           = (void*)0,
    [KISS_RNG]             = (void*)0,
    [MERSENNE_TWISTER_RNG] = &gsl_seed_double_RNG,
    [WELL_RNG]             = (void*)0,
};
seed_double_RNG_U   seed_double_RNG_Us  [NB_RNG] = {
    [BAD_RAND_RNG]         = &rand_seed_double_RNG,
    [BAD_KISS_RNG]         = (void*)0,
    [RAND_RNG]             = &rand_seed_double_RNG,
    [RAND48_RNG]           = (void*)0,
    [KISS_RNG]             = (void*)0,
    [MERSENNE_TWISTER_RNG] = (void*)0,
    [WELL_RNG]             = (void*)0,
};
seed_double_RNG_L   seed_double_RNG_Ls  [NB_RNG] = {
    [BAD_RAND_RNG]         = (void*)0,
    [BAD_KISS_RNG]         = (void*)0,
    [RAND_RNG]             = (void*)0,
    [RAND48_RNG]           = &rand48_seed_double_RNG,
    [KISS_RNG]             = (void*)0,
    [MERSENNE_TWISTER_RNG] = (void*)0,
    [WELL_RNG]             = (void*)0,
};
// Only the RNGs from the GSL have something to free at the end.
void nothing_to_delete(void) {}
delete_double_RNG delete_double_RNGs[NB_RNG] = {
    [BAD_RAND_RNG]         = &nothing_to_delete,
    [BAD_KISS_RNG]         = &nothing_to_delete,
    [RAND_RNG]             = &nothing_to_delete,
    [RAND48_RNG]           = &nothing_to_delete,
    [KISS_RNG]             = &nothing_to_delete,
    [MERSENNE_TWISTER_RNG] = &gsl_delete_double_RNG,
    [WELL_RNG]             = &nothing_to_delete,
};





/* ***************************************************************************** */
/* Copyright:      Francois Panneton and Pierre L'Ecuyer, University of Montreal */
/*                 Makoto Matsumoto, Hiroshima University                        */
/* Notice:         This code can be used freely for personal, academic,          */
/*                 or non-commercial purposes. For commercial purposes,          */
/*                 please contact P. L'Ecuyer at: lecuyer@iro.UMontreal.ca       */
/* ***************************************************************************** */

#define W 32
#define R 624
#define P 31
#define MASKU (0xffffffffU>>(W-P))
#define MASKL (~MASKU)
#define M1 70
#define M2 179
#define M3 449

#define MAT0POS(t,v) (v^(v>>t))
#define MAT0NEG(t,v) (v^(v<<(-(t))))
#define MAT1(v) v
#define MAT3POS(t,v) (v>>t)

/* To obtain the WELL19937c, uncomment the following line */
/* #define TEMPERING                                      */
#define TEMPERB 0xe46e1700U
#define TEMPERC 0x9b868000U

#define V0            STATE[state_i]
#define VM1Over       STATE[state_i+M1-R]
#define VM1           STATE[state_i+M1]
#define VM2Over       STATE[state_i+M2-R]
#define VM2           STATE[state_i+M2]
#define VM3Over       STATE[state_i+M3-R]
#define VM3           STATE[state_i+M3]
#define VRm1          STATE[state_i-1]
#define VRm1Under     STATE[state_i+R-1]
#define VRm2          STATE[state_i-2]
#define VRm2Under     STATE[state_i+R-2]

#define newV0         STATE[state_i-1]
#define newV0Under    STATE[state_i-1+R]
#define newV1         STATE[state_i]
#define newVRm1       STATE[state_i-2]
#define newVRm1Under  STATE[state_i-2+R]

#define FACT 2.32830643653869628906e-10

static int state_i = 0;
static unsigned int STATE[R];
static unsigned int z0, z1, z2;
static double case_1 (void);
static double case_2 (void);
static double case_3 (void);
static double case_4 (void);
static double case_5 (void);
static double case_6 (void);
       double (*WELLRNG19937a) (void);

#ifdef TEMPERING
static unsigned int y;
#endif

void InitWELLRNG19937a (unsigned int *init){
   int j;
   state_i = 0;
   WELLRNG19937a = case_1;
   for (j = 0; j < R; j++)
     STATE[j] = init[j];
}

double case_1 (void){
   // state_i == 0
   z0 = (VRm1Under & MASKL) | (VRm2Under & MASKU);
   z1 = MAT0NEG (-25, V0) ^ MAT0POS (27, VM1);
   z2 = MAT3POS (9, VM2) ^ MAT0POS (1, VM3);
   newV1      = z1 ^ z2;
   newV0Under = MAT1 (z0) ^ MAT0NEG (-9, z1) ^ MAT0NEG (-21, z2) ^ MAT0POS (21, newV1);
   state_i = R - 1;
   WELLRNG19937a = case_3;
#ifdef TEMPERING
   y = STATE[state_i] ^ ((STATE[state_i] << 7) & TEMPERB);
   y =              y ^ ((             y << 15) & TEMPERC);
   return ((double) y * FACT);
#else
   return ((double) STATE[state_i] * FACT);
#endif
}

static double case_2 (void){
   // state_i == 1
   z0 = (VRm1 & MASKL) | (VRm2Under & MASKU);
   z1 = MAT0NEG (-25, V0) ^ MAT0POS (27, VM1);
   z2 = MAT3POS (9, VM2) ^ MAT0POS (1, VM3);
   newV1 = z1 ^ z2;
   newV0 = MAT1 (z0) ^ MAT0NEG (-9, z1) ^ MAT0NEG (-21, z2) ^ MAT0POS (21, newV1);
   state_i = 0;
   WELLRNG19937a = case_1;
#ifdef TEMPERING
   y = STATE[state_i] ^ ((STATE[state_i] << 7) & TEMPERB);
   y =              y ^ ((             y << 15) & TEMPERC);
   return ((double) y * FACT);
#else
   return ((double) STATE[state_i] * FACT);
#endif
}

static double case_3 (void){
   // state_i+M1 >= R
   z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
   z1 = MAT0NEG (-25, V0) ^ MAT0POS (27, VM1Over);
   z2 = MAT3POS (9, VM2Over) ^ MAT0POS (1, VM3Over);
   newV1 = z1 ^ z2;
   newV0 = MAT1 (z0) ^ MAT0NEG (-9, z1) ^ MAT0NEG (-21, z2) ^ MAT0POS (21, newV1);
   state_i--;
   if (state_i + M1 < R)
      WELLRNG19937a = case_5;
#ifdef TEMPERING
   y = STATE[state_i] ^ ((STATE[state_i] << 7) & TEMPERB);
   y =              y ^ ((             y << 15) & TEMPERC);
   return ((double) y * FACT);
#else
   return ((double) STATE[state_i] * FACT);
#endif
}

static double case_4 (void){
   // state_i+M3 >= R
   z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
   z1 = MAT0NEG (-25, V0) ^ MAT0POS (27, VM1);
   z2 = MAT3POS (9, VM2) ^ MAT0POS (1, VM3Over);
   newV1 = z1 ^ z2;
   newV0 = MAT1 (z0) ^ MAT0NEG (-9, z1) ^ MAT0NEG (-21, z2) ^ MAT0POS (21, newV1);
   state_i--;
   if (state_i + M3 < R)
      WELLRNG19937a = case_6;
#ifdef TEMPERING
   y = STATE[state_i] ^ ((STATE[state_i] << 7) & TEMPERB);
   y =              y ^ ((             y << 15) & TEMPERC);
   return ((double) y * FACT);
#else
   return ((double) STATE[state_i] * FACT);
#endif
}

static double case_5 (void){
   // state_i+M2 >= R
   z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
   z1 = MAT0NEG (-25, V0) ^ MAT0POS (27, VM1);
   z2 = MAT3POS (9, VM2Over) ^ MAT0POS (1, VM3Over);
   newV1 = z1 ^ z2;
   newV0 = MAT1 (z0) ^ MAT0NEG (-9, z1) ^ MAT0NEG (-21, z2) ^ MAT0POS (21, newV1);
   state_i--;
   if (state_i + M2 < R)
      WELLRNG19937a = case_4;
#ifdef TEMPERING
   y = STATE[state_i] ^ ((STATE[state_i] << 7) & TEMPERB);
   y =              y ^ ((             y << 15) & TEMPERC);
   return ((double) y * FACT);
#else
   return ((double) STATE[state_i] * FACT);
#endif
}

static double case_6 (void){
   // 2 <= state_i <= (R - M3 - 1)
   z0 = (VRm1 & MASKL) | (VRm2 & MASKU);
   z1 = MAT0NEG (-25, V0) ^ MAT0POS (27, VM1);
   z2 = MAT3POS (9, VM2) ^ MAT0POS (1, VM3);
   newV1 = z1 ^ z2;
   newV0 = MAT1 (z0) ^ MAT0NEG (-9, z1) ^ MAT0NEG (-21, z2) ^ MAT0POS (21, newV1);
   state_i--;
   if (state_i == 1)
      WELLRNG19937a = case_2;
#ifdef TEMPERING
   y = STATE[state_i] ^ ((STATE[state_i] << 7) & TEMPERB);
   y =              y ^ ((             y << 15) & TEMPERC);
   return ((double) y * FACT);
#else
   return ((double) STATE[state_i] * FACT);
#endif
}

/*
 * Returns a 64-bit seed from time, process ID and host ID.
 * Also uses mpi_rank for the probable case where different MPI processes
 * are launched on the same machine.
 *
 * One could alternatively use bits from /dev/(u)random
 */
unsigned long int seed_64bits(int mpi_rank) {
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