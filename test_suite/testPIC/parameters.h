#ifndef PIC_VERT_PARAMETERS
#define PIC_VERT_PARAMETERS

#include "compiler_test.h" // constant PIC_VERT_C11

/*********************************
 * Real number helpers (printf). *
 *********************************/
// #include <float.h> // constant DBL_DECIMAL_DIG (17 : number of decimals a double has, only in C11)
                   // constant FLT_DECIMAL_DIG ( 9 : number of decimals a float  has, only in C11)
// According to https://en.wikipedia.org/wiki/Floating_point#Internal_representation
/* #if !defined(FLT_DECIMAL_DIG) */
const int FLT_DECIMAL_DIG = 9;
/* #endif */
/* #if !defined(DBL_DECIMAL_DIG) */
const int DBL_DECIMAL_DIG = 17;
/* #endif */


/****************************
 * Mathematical parameters. *
 ****************************/
// Pi = 4 * arctan(1)
const double PI = 3.14159265358979323846264338327;

// To test equality of two floating-point numbers a & b,
// instead  of using a == b, use fabs(a - b) < EPSILON.
#define EPSILON 0.0000000001


/****************************
 * Architecture parameters. *
 ****************************/
// Big Endian or Little Endian
// https://stackoverflow.com/questions/2100331/c-macro-definition-to-determine-big-endian-or-little-endian-machine
#define IS_BIG_ENDIAN (!*(unsigned char *)&(uint16_t){1})

// Signed Arithmetic
// http://icube-icps.unistra.fr/index.php/File:ModernC.pdf (5.5.5. Signed integers)
#define IS_TWO_S_COMPLEMENT ((-1&3) == 3)

// If the architecture supports vector size 512 (AVX-512 instructions)
#if defined(__AVX512F__)
const int VEC_ALIGN = 64;
// else if it supports vector size 256 (AVX instructions)
#elif defined(__AVX__)
const int VEC_ALIGN = 32;
// else probably it supports vector size 128 (SSE instructions).
#else
const int VEC_ALIGN = 16;
#endif

// Declare an aligned variable or struct field.
#if defined(PIC_VERT_C11)
#    define PIC_VERT_ALIGN(type, alignment) _Alignas(alignment) type
#else
#    define PIC_VERT_ALIGN(type, alignment) __attribute__((aligned(alignment))) type
#endif

// Cache line size, to avoid false sharing.
// https://stackoverflow.com/questions/794632/programmatically-get-the-cache-line-size
#define CACHE_LINE_SIZE 64

// Alignment needed to avoid false sharing *and* to enable aligned vectorization.
#define PIC_VERT_MAX_ALIGNMENT (CACHE_LINE_SIZE < VEC_ALIGN ? VEC_ALIGN : CACHE_LINE_SIZE)


/**************************
 * Simulation parameters. *
 **************************/
// Number of particles (most of the time per MPI process, currently in domain decomposition, in total)
/* #if !defined(NB_PARTICLE) */
const long int NB_PARTICLE = 50000000;
/* #endif */

// Grid size.
/* #if !defined(NCX) */
const int NCX = 64;
/* #endif */
/* #if !defined(NCY) */
const int NCY = 64;
/* #endif */
#if !defined(NCZ)
#   define NCZ 64
#endif

// Time step.
/* #if !defined(DELTA_T) */
const double DELTA_T = 0.1;
/* #endif */

// Number of total iterations.
/* #if !defined(NB_ITER) */
const int NB_ITER = 100;
/* #endif */

// Useful for chunk bags.
/* #if !defined(CHUNK_SIZE) */
const int CHUNK_SIZE = 512;
/* #endif */

#endif // ifndef PIC_VERT_PARAMETERS
