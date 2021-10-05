#ifndef PIC_VERT_PARAMETERS
#define PIC_VERT_PARAMETERS

// Pi = 4 * arctan(1)
#define PI 3.14159265358979323846264338327

// To test equality of two floating-point numbers a & b,
// instead  of using a == b, use fabs(a - b) < EPSILON.
#define EPSILON 0.0000000001

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
#define CACHE_LINE_SIZE 64

// Alignment needed to avoid false sharing *and* to enable aligned vectorization.
#define PIC_VERT_MAX_ALIGNMENT (CACHE_LINE_SIZE < VEC_ALIGN ? VEC_ALIGN : CACHE_LINE_SIZE)

// For most simulations, the number of particles can be passed at compile time.
#if !defined(NB_PARTICLE)
#   define NB_PARTICLE 50000000
#endif

// Useful for chunk bags.
#if !defined(CHUNK_SIZE)
#   define CHUNK_SIZE 512
#endif
#if !defined(NB_PROC)
#   define NB_PROC 2
#endif

#endif // ifndef PIC_VERT_PARAMETERS
