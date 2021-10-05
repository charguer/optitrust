#ifndef PIC_VERT_SPACE_FILLING_CURVES
#define PIC_VERT_SPACE_FILLING_CURVES

#include "math_functions.h" // function max, ceiling

enum SPACE_FILLING_CURVES {
    ROW_MAJOR,
    TILE,
    TILE_PRECALC,
    MORTON,
    MORTON_PRECALC,
    HILBERT,
    NB_CURVES // Always has to be last if you update this enum !
};

char space_filling_curves_names[NB_CURVES][30];

/*
 * Parameters :
 *     a. #define I_CELL_2D_TYPE - ordering of the cells (row major, tiles, Hilbert, Morton...)
 *     b. #define I_CELL_3D_TYPE - ordering of the cells (row major, tiles, Hilbert, Morton...)
 *     c. #define TILE_SIZE      - in case of tiling
 */
#if !defined(I_CELL_2D_TYPE)
#   define I_CELL_2D_TYPE ROW_MAJOR
#endif
#if !defined(I_CELL_3D_TYPE)
#   define I_CELL_3D_TYPE ROW_MAJOR
#endif
#if !defined(TILE_SIZE)
#   define TILE_SIZE 8
#endif

/*****************************************************************************
 *                           Dilated integers                                *
 *****************************************************************************/

/*
 * We're working on 32 bits. Thus :
 * evenBits = 01010101010101010101010101010101 (in binary)
 *  oddBits = 10101010101010101010101010101010 (in binary)
 *
 * We can note that oddBits = evenBits << 1.
 */
#define evenBits 0x55555555
#define  oddBits 0xaaaaaaaa

/*
 * In the following :
 *     e stands for a generic  even-dilated integer.
 *         (see for example ei)
 *         (carries the row index information)
 *     o stands for a generic   odd-dilated integer
 *         (see for example oj)
 *         (carries the column index information)
 *     m stands for a generic mixed-dilated integer
 *         (see for example mij)
 *         (the sum of an even-dilated and an odd-dilated integer)
 *         (carries both row and column indices information)
 *
 * Example :
 * i = 51 (in decimal) =  1 1 0 0 1 1 (in binary)
 * ei = even_of_int(i) = 010100000101 (in binary)
 * (put the bits of i on even indices, and 0 on  odd indices)
 * j = 45 (in decimal) = 1 0 1 1 0 1  (in binary)
 * oj =  odd_of_int(j) = 100010100010 (in binary)
 * (put the bits of j on  odd indices, and 0 on even indices)
 *
 * Note that the even_of_int function is named dilate_2 in the following,
 * and that odd_of_int(i) is just dilate_2(i) << 1.
 *
 * You can then add ei and oj to have a mixed-dilated integer
 * mij = ei + oj =   010100000101
 *                 + 100010100010
 *               =   110110100111 = 3495 (in decimal).
 *
 * Example of usage : if M is a a standard matrix and Z is its reverse(N)-Morton counterpart,
 * then M[51][45] = Z[3495].
 */
#define   even_of_odd(o) (o >> 1)
#define   odd_of_even(e) (e << 1)
#define even_of_mixed(m) (m & evenBits)
#define  odd_of_mixed(m) (m &  oddBits)

/*
 * We're working on 32 bits. Thus :
 * zeroBits = 01001001001001001001001001001001 (in binary)
 *  oneBits = 10010010010010010010010010010010 (in binary)
 *  twoBits = 00100100100100100100100100100100 (in binary)
 *
 * We can note that oneBits = zeroBits << 1 and twoBits = oneBits << 1.
 */
#define zeroBits 0x49249249
#define  oneBits 0x92492492
#define  twoBits 0x24924924

/*
 * In the following :
 *     z stands for a generic  zero-dilated integer.
 *         (see for example zi)
 *         (carries the row index information)
 *     o stands for a generic   one-dilated integer
 *         (see for example oj)
 *         (carries the column index information)
 *     t stands for a generic   two-dilated integer
 *         (see for example tk)
 *         (carries the rod index information)
 *     m stands for a generic mixed-dilated integer
 *         (see for example mijk)
 *         (the sum of a zero-dilated, a one-dilated and a two-dilated integer)
 *         (carries row, column and rod indices information)
 *
 * Example :
 * i = 51 (in decimal) =   1  1  0  0  1  1 (in binary)
 * zi = zero_of_int(i) = 001001000000001001 (in binary)
 * (put the bits of i on zero indices, and 0 on other indices)
 * j = 45 (in decimal) =  1  0  1  1  0  1  (in binary)
 * oj =  one_of_int(j) = 010000010010000010 (in binary)
 * (put the bits of j on  one indices, and 0 on other indices)
 * k = 23 (in decimal) =    1  0  1  1  1   (in binary)
 * tk =  two_of_int(k) = 000100000100100100 (in binary)
 * (put the bits of k on  two indices, and 0 on other indices)
 *
 * Note that the zero_of_int function is named dilate_3 in the following,
 *     that one_of_int(i) is just dilate_3(i) << 1
 * and that two_of_int(i) is just dilate_3(i) << 2.
 *
 * You can then add ei and oj to have a mixed-dilated integer
 * mijk = zi + oj + tk =   001001000000001001
 *                       + 010000010010000010
 *                       + 000100000100100100
 *                     =   011101010110101111 = 120239 (in decimal).
 */
#define zero_of_mixed(m) (m & zeroBits)
#define  one_of_mixed(m) (m &  oneBits)
#define  two_of_mixed(m) (m &  twoBits)

/*
 * Example of memory locations in a reverse(N)-Morton Matrix :
 *
 *  .------------------------> j
 *  | 00 02 08 10 32 34 40 42
 *  | 01 03 09 11 33 35 41 43
 *  | 04 06 12 14 36 38 44 46
 *  | 05 07 13 15 37 39 45 47
 *  | 16 18 24 26 48 50 56 58
 *  | 17 19 25 27 49 51 57 59
 *  | 20 22 28 30 52 54 60 62
 *  | 21 23 29 31 53 55 61 63
 *  V
 *  i
 *
 * If we call M[0..7][0..7] the standard matrix and Z[0..63] the reverse(N)-Morton Matrix,
 * it means that for example :
 *
 * M[0][0] = Z[0]
 * M[2][3] = Z[14]
 * M[7][6] = Z[61]
 *
 * In the general case, see next algorithms from Rajeev Raman and David S. Wise,
 * "Converting to and from Dilated Integers", IEEE, 2007.
 * http://www.cs.indiana.edu/~dswise/Arcee/castingDilated-comb.pdf
 */

const unsigned short int dilate_tab2[256];
const unsigned char undilate_tab2[256];
const unsigned char dilate_tab3[256];

/*
 * The two following algorithms use bitwise inclusive-or | to sum bit patterns.
 * Alternatives are simple addition + and bitwise exclusive-or ˆ because the
 * patterns are bitwise disjoint.
 *
 * unsigned short : integers between 0 and 2^16 - 1 = 65 535 (always on 16 bits)
 * unsigned int   : integers between 0 and 2^32 - 1 = 4 294 967 295 (on a 32 bits processor)
 *                  integers between 0 and 2^64-1 (roughly 2*10^19) (on a 64 bits processor)
 */
static inline unsigned int dilate_2(unsigned short x){
  return dilate_tab2[   0xFF & x     ]
      | (dilate_tab2[(0xFFFF & x) >>8] <<16);
}

static inline unsigned short undilate_2(unsigned int x){
  return undilate_tab2[0xFF & ((x>>7) |x)      ]
      | (undilate_tab2[0xFF &(((x>>7) |x) >>16)]
           << 8);
}

static inline unsigned int dilate_3(unsigned short x){
  return ((
      ((dilate_tab3[(0xFFFF & x) >>8]  )<<24)
     |  dilate_tab3[   0xFF & x     ]
    ) * 0x010101) & 0x49249249;
}

static inline unsigned short undilate_3(unsigned int x){
   return dilate_tab3[0xFF & ((((x>>8) | x) >>8)
                               | x)      ]
       | (dilate_tab3[0xFF &(((((x>>8) | x) >>8)
                              | x) >>24)] <<8);
}

/*
 * Useful for loops that use dilated integers.
 * evenIncrement is the counterpart of i+1 on row indices.
 *  oddIncrement is the counterpart of j+1 on column indices.
 */
#define evenIncrement(e) ((e - evenBits) & evenBits)
#define  oddIncrement(o) ((o -  oddBits) &  oddBits)

/*
 * Algorithms without lookup table.
 */
static inline unsigned int dilate_2_notable(unsigned short t){
  unsigned int r = t;
  r = (r | (r << 8)) & 0x00FF00FF;
  r = (r | (r << 4)) & 0x0F0F0F0F;
  r = (r | (r << 2)) & 0x33333333;
  r = (r | (r << 1)) & 0x55555555;
  return r;
}

static inline unsigned short undilate_2_notable(unsigned int t){
  t = (t *   3) & 0x66666666;
  t = (t *   5) & 0x78787878;
  t = (t *  17) & 0x7F807F80;
  t = (t * 257) & 0x7FFF8000;
  return ((unsigned short) (t >> 15));
}

static inline unsigned int dilate_3_notable(unsigned short t) {
    unsigned int r = t;
    r =  (r * 0x10001) & 0xFF0000FF; 
    r =  (r * 0x00101) & 0x0F00F00F;
    r =  (r * 0x00011) & 0xC30C30C3;
    r =  (r * 0x00005) & 0x49249249;
    return(r);
} 

static inline unsigned short undilate_3_notable(unsigned int t){
    t = (t * 0x00015) & 0x0E070381;
    t = (t * 0x01041) & 0x0FF80001;
    t = (t * 0x40001) & 0x0FFC0000;
    return ((unsigned short) (t >> 18));
}



#if (I_CELL_2D_TYPE == HILBERT) || (I_CELL_3D_TYPE == HILBERT)
/*****************************************************************************
 *                             Hilbert functions                             *
 *                                                                           *
 * The algorithms are adapted from John Skilling,                            *
 * "Programming the Hilbert curve", American Institute of Physics, 2004.     *
 * The only difference is that we need to have the index stored as a number, *
 * not stored in different numbers, so we use dilated integers in addition.  *
 *****************************************************************************/

/*
 * Example of memory locations in a Hilbert Matrix :
 *
 *  .------------------------> j
 *  | 00 03 04 05 58 59 60 63
 *  | 01 02 07 06 57 56 61 62
 *  | 14 13 08 09 54 55 50 49
 *  | 15 12 11 10 53 52 51 48
 *  | 16 17 30 31 32 33 46 47
 *  | 19 18 29 28 35 34 45 44
 *  | 20 23 24 27 36 39 40 43
 *  | 21 22 25 26 37 38 41 42
 *  V
 *  i
 *
 * If we call M[0..7][0..7] the standard matrix and H[0..63] the Hilbert Matrix,
 * it means that for example :
 *
 * M[0][0] = H[0]
 * M[2][3] = H[9]
 * M[7][6] = H[41]
 */

typedef unsigned int coord_t; // char,short,int for up to 8,16,32 bits per word

#if (I_CELL_2D_TYPE == HILBERT)
/*
 * @param  h, the code of the point (x, y) on the Hilbert curve.
 *         0 <= z < 4^r
 * @param  r the order of the Hilbert curve on which the point lies.
 * @return x the coordinate of the point to decode on the x-axis.
 *         0 <= x < 2^r, and
 *         y the coordinate of the point to decode on the y-axis.
 *         0 <= y < 2^r
 */
static void decodeHilbert2d(unsigned int h, unsigned char r, unsigned short* x, unsigned short* y) {
    coord_t max_pow_2, mask, pow_2, tmp;
    
    // Gray decode : H -> H ^ (H/2)
    tmp = h ^ (h >> 1);
    
    *x = undilate_2( odd_of_mixed(tmp) >> 1);
    *y = undilate_2(even_of_mixed(tmp)     );
    
    // Undo excess work
    max_pow_2 = 1 << r;
    for (pow_2 = 2; pow_2 != max_pow_2; pow_2 <<= 1) {
        mask = pow_2 - 1;
        if (*y & pow_2)   // invert
            *x ^= mask;
        else {            // exchange
            tmp = (*x ^ *y) & mask;
            *x ^= tmp;
            *y ^= tmp;
        }
        if (*x & pow_2)   // invert
            *x ^= mask;
    }
}

/*
 * @param  r the order of the Hilbert curve on which the point lies.
 * @param  x the coordinate of the point to encode on the x-axis.
 *         0 <= x < 2^r
 * @param  y the coordinate of the point to encode on the y-axis.
 *         0 <= y < 2^r
 * @return h, the code of the point (x, y) on the Hilbert curve.
 *         0 <= z < 4^r
 */
static unsigned int encodeHilbert2d(unsigned char r, unsigned short x, unsigned short y) {
    coord_t mask, pow_2, tmp;
    coord_t h_odd  = x;
    coord_t h_even = y;
    int     i;
    
    // Inverse undo
    for (pow_2 = 1 << (r - 1); pow_2 > 1; pow_2 >>= 1) {
        mask = pow_2 - 1;
        if (h_odd & pow_2)  // invert
            h_odd ^= mask;
        if (h_even & pow_2) // invert
            h_odd ^= mask;
        else {              // exchange
            tmp = (h_odd ^ h_even) & mask;
            h_odd  ^= tmp;
            h_even ^= tmp;
        }
    }
    
    // Gray encode : the inverse of H -> H ^ (H/2) can be computed
    // in log(r) operations as follows :
    h_even ^= h_odd;
    tmp = h_even;
    for (i = 1; i < r; i <<= 1)
        h_even ^= h_even >> i;
    tmp ^= h_even;
    h_odd ^= tmp;
    return dilate_2(h_even) + (dilate_2(h_odd) << 1);
}
#elif (I_CELL_3D_TYPE == HILBERT)
/*
 * @param  h, the code of the point (x, y, z) on the Hilbert curve.
 *         0 <= z < 8^r
 * @param  r the order of the Hilbert curve on which the point lies.
 * @return x the coordinate of the point to decode on the x-axis.
 *         0 <= x < 2^r,
 *         y the coordinate of the point to decode on the y-axis.
 *         0 <= y < 2^r, and
 *         z the coordinate of the point to decode on the z-axis.
 *         0 <= z < 2^r.
 */
static void decodeHilbert3d(unsigned int h, unsigned char r, unsigned short* x, unsigned short* y, unsigned short* z) {
    coord_t max_pow_2, mask, pow_2, tmp;
    
    // Gray decode : H -> H ^ (H/2)
    tmp = h ^ (h >> 1);
    
    *x = undilate_3( two_of_mixed(tmp) >> 2);
    *y = undilate_3( one_of_mixed(tmp) >> 1);
    *z = undilate_3(zero_of_mixed(tmp)     );
    
    // Undo excess work
    max_pow_2 = 1 << r;
    for (pow_2 = 2; pow_2 != max_pow_2; pow_2 <<= 1) {
        mask = pow_2 - 1;
        if (*z & pow_2)   // invert
            *x ^= mask;
        else {            // exchange
            tmp = (*x ^ *z) & mask;
            *x ^= tmp;
            *z ^= tmp;
        }
        if (*y & pow_2)   // invert
            *x ^= mask;
        else {            // exchange
            tmp = (*x ^ *y) & mask;
            *x ^= tmp;
            *y ^= tmp;
        }
        if (*x & pow_2)   // invert
            *x ^= mask;
    }
}

/*
 * @param  r the order of the Hilbert curve on which the point lies.
 * @param  x the coordinate of the point to encode on the x-axis.
 *         0 <= x < 2^r
 * @param  y the coordinate of the point to encode on the y-axis.
 *         0 <= y < 2^r
 * @param  z the coordinate of the point to encode on the z-axis.
 *         0 <= z < 2^r
 * @return h, the code of the point (x, y, z) on the Hilbert curve.
 *         0 <= z < 8^r
 */
static unsigned int encodeHilbert3d(unsigned char r, unsigned short x, unsigned short y, unsigned short z) {
    coord_t mask, pow_2, tmp;
    coord_t h_2 = z;
    coord_t h_1 = y;
    coord_t h_0 = x;
    int     i;
    
    // Inverse undo
    for (pow_2 = 1 << (r - 1); pow_2 > 1; pow_2 >>= 1) {
        mask = pow_2 - 1;
        if (h_0 & pow_2) // invert
            h_0 ^= mask;
        if (h_1 & pow_2) // invert
            h_0 ^= mask;
        else {           // exchange
            tmp = (h_0 ^ h_1) & mask;
            h_0 ^= tmp;
            h_1 ^= tmp;
        }
        if (h_2 & pow_2) // invert
            h_0 ^= mask;
        else {           // exchange
            tmp = (h_0 ^ h_2) & mask;
            h_0 ^= tmp;
            h_2 ^= tmp;
        }
    }
    
    // Gray encode : the inverse of H -> H ^ (H/2) can be computed
    // in log(r) operations as follows :
    h_1 ^= h_0;
    h_2 ^= h_1;
    tmp = 0;
    for (pow_2 = 1 << (r - 1); pow_2 > 1; pow_2 >>= 1)
        if (h_2 & pow_2)
            tmp ^= pow_2 - 1;
    return dilate_3(h_2 ^ tmp) + (dilate_3(h_1 ^ tmp) << 1) + (dilate_3(h_0 ^ tmp) << 2);
}
#endif

/*
 * This function returns the smallest order of the Hilbert curve needed to go
 * through all the grid points.
 *
 * @param  num_cells the largest number of grid cells in all directions.
 * @return ceiling(log_2(num_cells)).
 */
static unsigned char curveOrder(int num_cells) {
    unsigned char result = 0;
    
    while (num_cells > (1 << result))
        result++;
    return result;
}
#endif



/*****************************************************************************
 *                          Tile functions                                   *
 *    WARNING : only for grid = 128 x 128 and TILE_SIZE = 4, 8, 16           *
 *                    or grid = 512 x 512 and TILE_SIZE = 8                  *
 *    With other parameters, you have to compute yourself the precalc grid   *
 *    and put it here. Anyway it was just used for testing purposes and is   *
 *    not efficient. It is kept for reproducibility of our paper results:    *
 *    Barsamian, Hirstoaga, Violard : "Efficient Data Structures for a       *
 *    Hybrid Parallel and Vectorized Particle-in-Cell Code", 2017            *
 *    https://hal.inria.fr/INRIA/hal-01504645v3                              *
 *****************************************************************************/

const int tile_precalc_128x128_8[128];
const int tile_precalc_128x128_4[128];
const int tile_precalc_128x128_16[128];
const int tile_precalc_512x512_8[512];



/*****************************************************************************
 *                    Compute cell index from positions                      *
 *****************************************************************************/

/*
 * The assembly shows that compiling code using % gives more operations than
 * code using &. As long as TILE_SIZE, ncx, ncy, ncz are powers of two, we can
 * replace modulos by bitwise ands directly into the code to help the compiler.
 */

#define TILE_SIZE_MINUS_ONE (TILE_SIZE - 1)

#if (I_CELL_2D_TYPE == ROW_MAJOR) // Row-major
#  define COMPUTE_I_CELL_2D(ncy, i_cell_x, i_cell_y) \
    ((i_cell_x) * ncy + (i_cell_y))
static inline int I_CELL_PARAM_2D(int ncx, int ncy) { return ncy; }
#elif (I_CELL_2D_TYPE == TILE) // Tile (no precalc)
#  define COMPUTE_I_CELL_2D(num_cells_per_column, i_cell_x, i_cell_y) \
    (TILE_SIZE * (i_cell_x) + ((i_cell_y) & TILE_SIZE_MINUS_ONE) + num_cells_per_column * ((i_cell_y) / TILE_SIZE))
static inline int I_CELL_PARAM_2D(int ncx, int ncy) { return ncx * TILE_SIZE; }
#elif (I_CELL_2D_TYPE == TILE_PRECALC) // Tile (with precalc - see comments)
#  define COMPUTE_I_CELL_2D(precalc_table, i_cell_x, i_cell_y) \
    (TILE_SIZE * (i_cell_x) + ((i_cell_y) & TILE_SIZE_MINUS_ONE) + num_cells_per_column * ((i_cell_y) / TILE_SIZE))
static inline int I_CELL_PARAM_2D(int ncx, int ncy) { return ncx * TILE_SIZE; }
/*
 * The code above doesn't use precalc, it is the same as Tile without precalc.
 * These computations cannot easily be written as the others. If you want to test
 * the precalc computations, you have to manually replace the COMPUTE_I_CELL_2D macro call
 * by the following :
    (TILE_SIZE * (i_cell_x) + precalc_table[i_cell_y])
 * where precalc_table has to be chosen among the 4 precomputed arrays,
 * or precomputed inside the program.
    (ncx == 128 && ncy == 128 \
        ? (TILE_SIZE == 8  ? tile_precalc_128x128_8 \
         : TILE_SIZE == 4  ? tile_precalc_128x128_4 \
         : TILE_SIZE == 16 ? tile_precalc_128x128_16 : no_precalc_available) \
        : (ncx == 512 && ncy == 512 && TILE_SIZE == 8 ? tile_precalc_512x512_8 : no_precalc_available))
 */
#elif (I_CELL_2D_TYPE == MORTON) // Morton (no lookup table)
#  define COMPUTE_I_CELL_2D(throw_away, i_cell_x, i_cell_y) \
    (dilate_2_notable(i_cell_x) + (dilate_2_notable(i_cell_y) << 1))
static inline int I_CELL_PARAM_2D(int ncx, int ncy) { return 0; }
#elif (I_CELL_2D_TYPE == MORTON_PRECALC) // Morton (with lookup table)
#  define COMPUTE_I_CELL_2D(throw_away, i_cell_x, i_cell_y) \
    (dilate_2(i_cell_x) + (dilate_2(i_cell_y) << 1))
static inline int I_CELL_PARAM_2D(int ncx, int ncy) { return 0; }
#elif (I_CELL_2D_TYPE == HILBERT) // Hilbert (auxiliary using Morton with lookup)
#  define COMPUTE_I_CELL_2D(curve_order, i_cell_x, i_cell_y) \
    encodeHilbert2d(curve_order, i_cell_x, i_cell_y)
static inline int I_CELL_PARAM_2D(int ncx, int ncy) { return curveOrder(max(ncx, ncy)); }
#endif



#if (I_CELL_3D_TYPE == ROW_MAJOR) // Row-major
#  define COMPUTE_I_CELL_3D(ncy, ncz, i_cell_x, i_cell_y, i_cell_z) \
    (((i_cell_x) * ncy + (i_cell_y)) * ncz + (i_cell_z))
static inline int I_CELL_PARAM1_3D(int ncx, int ncy, int ncz) { return ncy; }
static inline int I_CELL_PARAM2_3D(int ncx, int ncy, int ncz) { return ncz; }
#elif (I_CELL_3D_TYPE == TILE) // Tile
# if (is_power_of_two(TILE_SIZE))
#  define COMPUTE_I_CELL_3D(num_cells_per_tower, num_cells_per_wall, i_cell_x, i_cell_y, i_cell_z) \
    (((i_cell_x) / TILE_SIZE) * num_cells_per_tower + \
     ((i_cell_y) / TILE_SIZE) * num_cells_per_wall  + \
     (i_cell_z) * TILE_SIZE * TILE_SIZE + ((i_cell_y) & TILE_SIZE_MINUS_ONE) * TILE_SIZE + ((i_cell_x) & TILE_SIZE_MINUS_ONE))
# else
#  define COMPUTE_I_CELL_3D(num_cells_per_tower, num_cells_per_wall, i_cell_x, i_cell_y, i_cell_z) \
    (((i_cell_x) / TILE_SIZE) * num_cells_per_tower + \
     ((i_cell_y) / TILE_SIZE) * num_cells_per_wall  + \
     (i_cell_z) * TILE_SIZE * TILE_SIZE + ((i_cell_y) % TILE_SIZE) * TILE_SIZE + ((i_cell_x) % TILE_SIZE))
# endif
static inline int I_CELL_PARAM1_3D(int ncx, int ncy, int ncz) { return ncz * TILE_SIZE * TILE_SIZE; }
static inline int I_CELL_PARAM2_3D(int ncx, int ncy, int ncz) { return ncz * TILE_SIZE * TILE_SIZE * ceiling(ncx, TILE_SIZE); }
#elif (I_CELL_3D_TYPE == MORTON) // Morton (no lookup table)
#  define COMPUTE_I_CELL_3D(throw_away1, throw_away2, i_cell_x, i_cell_y, i_cell_z) \
    (dilate_3_notable(i_cell_x) + (dilate_3_notable(i_cell_y) << 1) + (dilate_3_notable(i_cell_z) << 2))
static inline int I_CELL_PARAM1_3D(int ncx, int ncy, int ncz) { return 0; }
static inline int I_CELL_PARAM2_3D(int ncx, int ncy, int ncz) { return 0; }
#elif (I_CELL_3D_TYPE == MORTON_PRECALC) // Morton (with lookup table)
#  define COMPUTE_I_CELL_3D(throw_away1, throw_away2, i_cell_x, i_cell_y, i_cell_z) \
    (dilate_3(i_cell_x) + (dilate_3(i_cell_y) << 1) + (dilate_3(i_cell_z) << 2))
static inline int I_CELL_PARAM1_3D(int ncx, int ncy, int ncz) { return 0; }
static inline int I_CELL_PARAM2_3D(int ncx, int ncy, int ncz) { return 0; }
#elif (I_CELL_3D_TYPE == HILBERT) // Hilbert (auxiliary using Morton with lookup)
#  define COMPUTE_I_CELL_3D(curve_order, throw_away, i_cell_x, i_cell_y, i_cell_z) \
    encodeHilbert3d(curve_order, i_cell_x, i_cell_y, i_cell_z)
static inline int I_CELL_PARAM1_3D(int ncx, int ncy, int ncz) { return curveOrder(max(ncx, max(ncy, ncz))); }
static inline int I_CELL_PARAM2_3D(int ncx, int ncy, int ncz) { return 0; }
#endif



/*****************************************************************************
 *                    Compute positions from cell index                      *
 *                         (direction by direction)                          *
 *****************************************************************************/

#if (I_CELL_2D_TYPE == ROW_MAJOR) // Row-major
#  define ICX_FROM_I_CELL_2D(ncy, i_cell)                            (i_cell / ncy)
static inline int ICX_PARAM_2D(int ncx, int ncy)                     { return ncy; }
#elif (I_CELL_2D_TYPE == TILE) || (I_CELL_2D_TYPE == TILE_PRECALC) // Tile
#  define ICX_FROM_I_CELL_2D(num_cells_per_column_minus_one, i_cell) ((i_cell & num_cells_per_column_minus_one) / TILE_SIZE)
static inline int ICX_PARAM_2D(int ncx, int ncy)                     { return ncx * TILE_SIZE - 1; }
#elif (I_CELL_2D_TYPE == MORTON) // Morton (no lookup table)
#  define ICX_FROM_I_CELL_2D(throw_away, i_cell)                     undilate_2_notable(even_of_mixed(i_cell))
static inline int ICX_PARAM_2D(int ncx, int ncy)                     { return 0; }
#else
// For Morton (with lookup table) or Hilbert (auxiliary using Morton with lookup),
// these functions are useless as they are not vectorizable.
static inline int ICX_PARAM_2D(int ncx, int ncy)                     { return 0; }
#endif

#if (I_CELL_2D_TYPE == ROW_MAJOR) // Row-major
#  define ICY_FROM_I_CELL_2D(ncyminusone, i_cell)          (i_cell & ncyminusone)
static inline int ICY_PARAM_2D(int ncx, int ncy)           { return ncy - 1; }
#elif (I_CELL_2D_TYPE == TILE) || (I_CELL_2D_TYPE == TILE_PRECALC) // Tile
#  define ICY_FROM_I_CELL_2D(num_cells_per_column, i_cell) ((i_cell & TILE_SIZE_MINUS_ONE) + TILE_SIZE * (i_cell / num_cells_per_column))
static inline int ICY_PARAM_2D(int ncx, int ncy)           { return ncx * TILE_SIZE; }
#elif (I_CELL_2D_TYPE == MORTON) // Morton (no lookup table)
#  define ICY_FROM_I_CELL_2D(throw_away, i_cell)           undilate_2_notable( odd_of_mixed(i_cell) >> 1)
static inline int ICY_PARAM_2D(int ncx, int ncy)           { return 0; }
#else
// For Morton (with lookup table) or Hilbert (auxiliary using Morton with lookup),
// these functions are useless as they are not vectorizable.
static inline int ICY_PARAM_2D(int ncx, int ncy)           { return 0; }
#endif



#define SQR_TILE_SIZE_MINUS_ONE (TILE_SIZE * TILE_SIZE - 1)

#if (I_CELL_3D_TYPE == ROW_MAJOR) // Row-major
#  define ICX_FROM_I_CELL_3D(ncy, ncz, i_cell)                                          ((i_cell / ncz) / ncy)
static inline int ICX_PARAM1_3D(int ncx, int ncy, int ncz)                              { return ncy; }
static inline int ICX_PARAM2_3D(int ncx, int ncy, int ncz)                              { return ncz; }
#elif (I_CELL_3D_TYPE == TILE) // Tile
# if (is_power_of_two(TILE_SIZE))
#  define ICX_FROM_I_CELL_3D(num_cells_per_tower, num_cells_per_wall_minus_one, i_cell) (((i_cell & num_cells_per_wall_minus_one) / num_cells_per_tower) * TILE_SIZE + (i_cell & TILE_SIZE_MINUS_ONE))
static inline int ICX_PARAM2_3D(int ncx, int ncy, int ncz)                              { return ncz * TILE_SIZE * TILE_SIZE * ceiling(ncx, TILE_SIZE) - 1; }
# else
#  define ICX_FROM_I_CELL_3D(num_cells_per_tower, num_cells_per_wall          , i_cell) (((i_cell % num_cells_per_wall          ) / num_cells_per_tower) * TILE_SIZE + (i_cell % TILE_SIZE))
static inline int ICX_PARAM2_3D(int ncx, int ncy, int ncz)                              { return ncz * TILE_SIZE * TILE_SIZE * ceiling(ncx, TILE_SIZE); }
# endif
static inline int ICX_PARAM1_3D(int ncx, int ncy, int ncz)                              { return ncz * TILE_SIZE * TILE_SIZE; }
#elif (I_CELL_3D_TYPE == MORTON) // Morton (no lookup table)
#  define ICX_FROM_I_CELL_3D(throw_away1, throw_away2, i_cell)                          undilate_3_notable(zero_of_mixed(i_cell))
static inline int ICX_PARAM1_3D(int ncx, int ncy, int ncz)                              { return 0; }
static inline int ICX_PARAM2_3D(int ncx, int ncy, int ncz)                              { return 0; }
#else
// For Morton (with lookup table) or Hilbert (auxiliary using Morton with lookup),
// these functions are useless as they are not vectorizable.
static inline int ICX_PARAM1_3D(int ncx, int ncy, int ncz)                              { return 0; }
static inline int ICX_PARAM2_3D(int ncx, int ncy, int ncz)                              { return 0; }
#endif

#if (I_CELL_3D_TYPE == ROW_MAJOR) // Row-major
#  define ICY_FROM_I_CELL_3D(ncyminusone, ncz, i_cell)               ((i_cell / ncz) & ncyminusone)
static inline int ICY_PARAM1_3D(int ncx, int ncy, int ncz)           { return ncy - 1; }
static inline int ICY_PARAM2_3D(int ncx, int ncy, int ncz)           { return ncz; }
#elif (I_CELL_3D_TYPE == TILE) // Tile
# if (is_power_of_two(TILE_SIZE))
#  define ICY_FROM_I_CELL_3D(num_cells_per_wall, throw_away, i_cell) ((i_cell / num_cells_per_wall) * TILE_SIZE + (i_cell & SQR_TILE_SIZE_MINUS_ONE) / TILE_SIZE)
# else
#  define ICY_FROM_I_CELL_3D(num_cells_per_wall, throw_away, i_cell) ((i_cell / num_cells_per_wall) * TILE_SIZE + (i_cell % (TILE_SIZE * TILE_SIZE)) / TILE_SIZE)
# endif
static inline int ICY_PARAM1_3D(int ncx, int ncy, int ncz)           { return ncz * TILE_SIZE * TILE_SIZE * ceiling(ncx, TILE_SIZE); }
static inline int ICY_PARAM2_3D(int ncx, int ncy, int ncz)           { return 0; }
#elif (I_CELL_3D_TYPE == MORTON) // Morton (no lookup table)
#  define ICY_FROM_I_CELL_3D(throw_away1, throw_away2, i_cell)       undilate_3_notable( one_of_mixed(i_cell) >> 1)
static inline int ICY_PARAM1_3D(int ncx, int ncy, int ncz)           { return 0; }
static inline int ICY_PARAM2_3D(int ncx, int ncy, int ncz)           { return 0; }
#else
// For Morton (with lookup table) or Hilbert (auxiliary using Morton with lookup),
// these functions are useless as they are not vectorizable.
static inline int ICY_PARAM1_3D(int ncx, int ncy, int ncz)           { return 0; }
static inline int ICY_PARAM2_3D(int ncx, int ncy, int ncz)           { return 0; }
#endif

#if (I_CELL_3D_TYPE == ROW_MAJOR) // Row-major
#  define ICZ_FROM_I_CELL_3D(nczminusone, i_cell)                   (i_cell & nczminusone)
static inline int ICZ_PARAM_3D(int ncx, int ncy, int ncz)           { return ncz - 1; }
#elif (I_CELL_3D_TYPE == TILE) // Tile
# if (is_power_of_two(TILE_SIZE))
#  define ICZ_FROM_I_CELL_3D(num_cells_per_tower_minus_one, i_cell) ((i_cell & num_cells_per_tower_minus_one) / (TILE_SIZE * TILE_SIZE))
static inline int ICZ_PARAM_3D(int ncx, int ncy, int ncz)           { return ncz * TILE_SIZE * TILE_SIZE - 1; }
# else
#  define ICZ_FROM_I_CELL_3D(num_cells_per_tower          , i_cell) ((i_cell % num_cells_per_tower          ) / (TILE_SIZE * TILE_SIZE))
static inline int ICZ_PARAM_3D(int ncx, int ncy, int ncz)           { return ncz * TILE_SIZE * TILE_SIZE; }
# endif
#elif (I_CELL_3D_TYPE == MORTON) // Morton (no lookup table)
#  define ICZ_FROM_I_CELL_3D(throw_away, i_cell)                    undilate_3_notable( two_of_mixed(i_cell) >> 2)
static inline int ICZ_PARAM_3D(int ncx, int ncy, int ncz)           { return 0; }
#else
// For Morton (with lookup table) or Hilbert (auxiliary using Morton with lookup),
// these functions are useless as they are not vectorizable.
static inline int ICZ_PARAM_3D(int ncx, int ncy, int ncz)           { return 0; }
#endif



/*****************************************************************************
 *                    Compute positions from cell index                      *
 *                         (all directions at once)                          *
 *           The parameters to pass for those functions are :                *
 *    I_CELL_PARAM_2D in 2d | (I_CELL_PARAM1_3D, I_CELL_PARAM2_3D) in 3d     *
 *****************************************************************************/

#if (I_CELL_2D_TYPE == ROW_MAJOR) // Row-major
static void REVERSE_I_CELL_2D(int ncy, int i_cell, short int* icx, short int* icy) {
    *icx = i_cell / ncy;
    *icy = i_cell % ncy;
}
#elif (I_CELL_2D_TYPE == TILE) || (I_CELL_2D_TYPE == TILE_PRECALC) // Tile
static void REVERSE_I_CELL_2D(int num_cells_per_column, int i_cell, short int* icx, short int* icy) {
    *icx = (i_cell % num_cells_per_column) / TILE_SIZE;
    *icy = (i_cell % TILE_SIZE) + TILE_SIZE * (i_cell / num_cells_per_column);
}
#elif (I_CELL_2D_TYPE == MORTON) // Morton (no lookup table)
static void REVERSE_I_CELL_2D(int throw_away, int i_cell, short int* icx, short int* icy) {
    *icx = undilate_2_notable(even_of_mixed(i_cell)     );
    *icy = undilate_2_notable( odd_of_mixed(i_cell) >> 1);
}
#elif (I_CELL_2D_TYPE == MORTON_PRECALC) // Morton (with lookup table)
static void REVERSE_I_CELL_2D(int throw_away, int i_cell, short int* icx, short int* icy) {
    *icx = undilate_2(even_of_mixed(i_cell)     );
    *icy = undilate_2( odd_of_mixed(i_cell) >> 1);
}
#elif (I_CELL_2D_TYPE == HILBERT) // Hilbert (auxiliary using Morton with lookup)
#define REVERSE_I_CELL_2D(curve_order, i_cell, i_cell_x, i_cell_y) \
    decodeHilbert2d(i_cell, curve_order, i_cell_x, i_cell_y)
#endif



#if (I_CELL_3D_TYPE == ROW_MAJOR) // Row-major
static void REVERSE_I_CELL_3D(int ncy, int ncz, int i_cell, short int* icx, short int* icy, short int* icz) {
    *icx = (i_cell / ncz) / ncy;
    *icy = (i_cell / ncz) % ncy;
    *icz =  i_cell % ncz;
}
#elif (I_CELL_3D_TYPE == TILE) // Tile
static void REVERSE_I_CELL_3D(int num_cells_per_tower, int num_cells_per_wall, int i_cell, short int* icx, short int* icy, short int* icz) {
    *icx = ((i_cell % num_cells_per_wall) / num_cells_per_tower) * TILE_SIZE + (i_cell % TILE_SIZE);
    *icy =  (i_cell / num_cells_per_wall) * TILE_SIZE + (i_cell % (TILE_SIZE * TILE_SIZE)) / TILE_SIZE;
    *icz =  (i_cell % num_cells_per_tower) / (TILE_SIZE * TILE_SIZE);
}
#elif (I_CELL_3D_TYPE == MORTON) // Morton (no lookup table)
static void REVERSE_I_CELL_3D(int throw_away1, int throw_away2, int i_cell, short int* icx, short int* icy, short int* icz) {
    *icx = undilate_3_notable(zero_of_mixed(i_cell)     );
    *icy = undilate_3_notable( one_of_mixed(i_cell) >> 1);
    *icz = undilate_3_notable( two_of_mixed(i_cell) >> 2);
}
#elif (I_CELL_3D_TYPE == MORTON_PRECALC) // Morton (with lookup table)
static void REVERSE_I_CELL_3D(int throw_away1, int throw_away2, int i_cell, short int* icx, short int* icy, short int* icz) {
    *icx = undilate_3(zero_of_mixed(i_cell)     );
    *icy = undilate_3( one_of_mixed(i_cell) >> 1);
    *icz = undilate_3( two_of_mixed(i_cell) >> 2);
}
#elif (I_CELL_3D_TYPE == HILBERT) // Hilbert (auxiliary using Morton with lookup)
#define REVERSE_I_CELL_3D(curve_order, throw_away, i_cell, i_cell_x, i_cell_y, i_cell_z) \
    decodeHilbert3d(i_cell, curve_order, i_cell_x, i_cell_y, i_cell_z)
#endif

#endif // ifndef PIC_VERT_SPACE_FILLING_CURVES

