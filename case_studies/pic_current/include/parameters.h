#ifndef PIC_VERT_PARAMETERS
#define PIC_VERT_PARAMETERS

/*********************************
 * Real number helpers (printf). *
 *********************************/
#include <float.h> // constant DBL_DECIMAL_DIG (17 : number of decimals a double has, only in C11)
                   // constant FLT_DECIMAL_DIG ( 9 : number of decimals a float  has, only in C11)
// According to https://en.wikipedia.org/wiki/Floating_point#Internal_representation
#if !defined(FLT_DECIMAL_DIG)
#    define FLT_DECIMAL_DIG 9
#endif
#if !defined(DBL_DECIMAL_DIG)
#    define DBL_DECIMAL_DIG 17
#endif


/****************************
 * Mathematical parameters. *
 ****************************/
// Pi = 4 * arctan(1)
#define PI 3.14159265358979323846264338327

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
#    define VEC_ALIGN 64
// else if it supports vector size 256 (AVX instructions)
#elif defined(__AVX__)
#    define VEC_ALIGN 32
// else probably it supports vector size 128 (SSE instructions).
#else
#    define VEC_ALIGN 16
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
#if !defined(NB_PARTICLE)
#   define NB_PARTICLE 50000000
#endif

// Grid size.
#if !defined(NCX)
#   define NCX 64
#endif
#if !defined(NCY)
#   define NCY 64
#endif
#if !defined(NCZ)
#   define NCZ 64
#endif

// Time step.
#if !defined(DELTA_T)
#   define DELTA_T 0.1
#endif

// Number of total iterations.
#if !defined(NB_ITER)
#   define NB_ITER 100
#endif

// TODO: comment what it does
#ifndef THERMAL_SPEED
#    define THERMAL_SPEED 1.
#endif

// TODO: comment what it does
#ifndef DRIFT_VELOCITY
#    define DRIFT_VELOCITY 4.
#endif

// TODO: comment what it does
#ifndef PROPORTION_FAST_PARTICLES
#    define PROPORTION_FAST_PARTICLES 0.01
#endif

// TODO: comment what it does
#ifndef INITIAL_DISTRIBUTION
#    define INITIAL_DISTRIBUTION LANDAU_3D_PROD_OF_ONE_PLUS_COS
#endif

// Useful for chunk bags.
#if !defined(CHUNK_SIZE)
#   define CHUNK_SIZE 512
#endif

#endif // ifndef PIC_VERT_PARAMETERS
