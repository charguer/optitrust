#ifndef PIC_VERT_MATH_FUNCTIONS
#define PIC_VERT_MATH_FUNCTIONS

#include <math.h>          // function floor
#include <stdbool.h>       // type     bool
#include "compiler_test.h" // constant PIC_VERT_OPENMP_4_0

/*
 * Computes the square of a double.
 *
 * @param  a.
 * @return a^2.
 */
#ifdef PIC_VERT_OPENMP_4_0
#pragma omp declare simd
#endif
static inline double sqr(double a) {
    return (a * a);
}

/*
 * The modulo (a % b) will output a value in {0, ..., b - 1} if (a >= 0)... but
 * will output a value in {1 - b, ..., 0} if (a < 0) in some implementations
 * (i.e. the sign of (a % b) is the sign of a), because the remainder is
 * defined via the equality (a / b) * b + a % b = a
 *
 * On floating-points, fmod(a, b) or remainder(a, b) also return a number that
 * is the sign of a.
 *
 * Thus we provide a new function that returns a non-negative value whatever
 * the sign of a (the modulo function in Fortran is exactly coded that way).
 *
 * @param[in] a.
 * @param[in] b.
 * @return    a - floor(a / b) * b, which corresponds to the idea of the modulo
 *            on real variables.
 */
#ifdef PIC_VERT_OPENMP_4_0
#pragma omp declare simd
#endif
static inline double modulo(double a, double b) {
    return a - floor(a / b) * b;
}

/*
 * Computes the maximum of two ints.
 *
 * @param  a, b.
 * @return max(a, b).
 */
static inline int max(int a, int b) {
    return a > b ? a : b;
}

/*
 * Computes the minimum of two ints.
 *
 * @param  a, b.
 * @return min(a, b).
 */
static inline int min(int a, int b) {
    return a < b ? a : b;
}

/*
 * Integer exponentiation.
 *
 * @param[in] base is the number to get exponentiated.
 * @param[in] exp is the exponent
 * @return    base**exp = base^exp
 */
static inline int int_pow(int base, int exp) {
    int result = 1;
    while (exp) {
        if (exp & 1)
            result *= base;
        exp /= 2;
        base *= base;
    }
    return result;
}

/*
 * Return the ceiling of a / b, as an integer (ceil function from math.h returns a double).
 *
 * @param  a, b
 * @return |¯ a / b ¯|.
 */
static inline int ceiling(long int a, long int b) {
    return (a + b - 1) / b;
}

/*
 * Return true iff a is an even integer.
 *
 * @param  a
 * @return (a is an even integer).
 */
static inline bool is_even(int a) {
    return a % 2 == 0;
}

/*
 * Return true iff a is a strictly positive power of two.
 * http://graphics.stanford.edu/~seander/bithacks.html#DetermineIfPowerOf2
 *
 * @param  a
 * @return (a is a strictly powitive power of two).
 */
static inline bool is_power_of_two(int a) {
    return a && !(a & (a - 1));
}

/*
 * Return the lowest even integer number greater of equal than n.
 *
 * @param  n
 * @return n when n is even, (n+1) when n is odd.
 */
static inline int lowest_even_number_greater_or_equal_than(int n) {
    return is_even(n) ? n : n + 1;
}

/*
 * Computes the average of an array of ints.
 *
 * @param  array[size].
 * @return (array[0] + ... + array[size-1]) / size.
 */
static inline double int_array_avg(int* array, int size) {
    double sum = (double)array[0] / ((double)size);
    for (int i = 1; i < size; i++)
        sum += (double)array[i] / ((double)size);
    return sum;
}

/*
 * Computes the maximum of an array of ints.
 *
 * @param  array[size].
 * @return max(array[0], ... array[size-1]).
 */
static inline int int_array_max(int* array, int size) {
    int max = array[0];
    for (int i = 1; i < size; i++)
        if (array[i] > max)
            max = array[i];
    return max;
}

/*
 * Computes the minimum of an array of ints.
 *
 * @param  array[size].
 * @return min(array[0], ... array[size-1]).
 */
static inline int int_array_min(int* array, int size) {
    int min = array[0];
    for (int i = 1; i < size; i++)
        if (array[i] < min)
            min = array[i];
    return min;
}

#endif // ifndef PIC_VERT_MATH_FUNCTIONS

