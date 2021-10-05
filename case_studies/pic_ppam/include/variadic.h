#ifndef VARIADIC

/*
 * Default arguments for C99. From Jens's blog:
 * https://gustedt.wordpress.com/2010/06/03/default-arguments-for-c99/
 * Improved answer :
 * https://stackoverflow.com/questions/1472138/c-default-arguments#33786937
 *
 * Examples of use in hdf5_io.h, output.h, particle_type_soa_2d.h, particle_type_soa_3d.h
 */

#define _NARG2(_0, _1, _2, ...) _2
#define NUMARG2(...) _NARG2(__VA_ARGS__, 2, 1, 0)
#define _NARG3(_0, _1, _2, _3, ...) _3
#define NUMARG3(...) _NARG3(__VA_ARGS__, 3, 2, 1, 0)
#define _NARG4(_0, _1, _2, _3, _4, ...) _4
#define NUMARG4(...) _NARG4(__VA_ARGS__, 4, 3, 2, 1, 0)
#define _NARG5(_0, _1, _2, _3, _4, _5, ...) _5
#define NUMARG5(...) _NARG5(__VA_ARGS__, 5, 4, 3, 2, 1, 0)
#define _NARG6(_0, _1, _2, _3, _4, _5, _6, ...) _6
#define NUMARG6(...) _NARG6(__VA_ARGS__, 6, 5, 4, 3, 2, 1, 0)
#define _NARG7(_0, _1, _2, _3, _4, _5, _6, _7, ...) _7
#define NUMARG7(...) _NARG7(__VA_ARGS__, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG8(_0, _1, _2, _3, _4, _5, _6, _7, _8, ...) _8
#define NUMARG8(...) _NARG8(__VA_ARGS__, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG9(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, ...) _9
#define NUMARG9(...) _NARG9(__VA_ARGS__, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG10(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, ...) _10
#define NUMARG10(...) _NARG10(__VA_ARGS__, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG11(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, ...) _11
#define NUMARG11(...) _NARG11(__VA_ARGS__, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG12(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, ...) _12
#define NUMARG12(...) _NARG12(__VA_ARGS__, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG13(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, ...) _13
#define NUMARG13(...) _NARG13(__VA_ARGS__, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG14(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, ...) _14
#define NUMARG14(...) _NARG14(__VA_ARGS__, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG15(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, ...) _15
#define NUMARG15(...) _NARG15(__VA_ARGS__, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG16(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, ...) _16
#define NUMARG16(...) _NARG16(__VA_ARGS__, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define _NARG17(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, ...) _17
#define NUMARG17(...) _NARG17(__VA_ARGS__, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define __VARIADIC(name, num_args, ...) name ## _ ## num_args (__VA_ARGS__)
#define _VARIADIC(name, num_args, ...) name (__VARIADIC(name, num_args, __VA_ARGS__))
#define VARIADIC(name, num_args, ...) _VARIADIC(name, num_args, __VA_ARGS__)

#endif // ifndef VARIADIC

