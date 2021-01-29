#ifndef VARIADIC

/*
 * Default arguments for C99. From Jens's blog:
 * https://gustedt.wordpress.com/2010/06/03/default-arguments-for-c99/
 * Improved answer without inline function and with any number of arguments:
 * https://stackoverflow.com/questions/1472138/c-default-arguments#33786937
 * Jens's improvement to make it work with empty arguments:
 * https://gustedt.wordpress.com/2010/06/08/detect-empty-macro-arguments/
 *
 * Examples of use in diagnostics.h, hdf5_io.h, matrix_functions.h, particle_type_soa_2d.h, particle_type_soa_3d.h
 *
 * Note: Can be extended to cope up with more arguments.
 */

#define _NARG32(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9, _10, _11, _12, _13, _14, _15, _16, _17, _18, _19, _20, _21, _22, _23, _24, _25, _26, _27, _28, _29, _30, _31, _32, ...) _32
#define NUMARG32(...) _NARG32(__VA_ARGS__, 32, 31, 30, 29, 28, 27, 26, 25, 24, 23, 22, 21, 20, 19, 18, 17, 16, 15, 14, 13, 12, 11, 10, 9, 8, 7, 6, 5, 4, 3, 2, 1, 0)
#define __VARIADIC(name, num_args, ...) name ## _ ## num_args (__VA_ARGS__)
#define _VARIADIC(name, num_args, ...) name (__VARIADIC(name, num_args, __VA_ARGS__))
#define VARIADIC(name, num_args, ...) _VARIADIC(name, num_args, __VA_ARGS__)

#define HAS_COMMA(...) _NARG32(__VA_ARGS__, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0)
#define _TRIGGER_PARENTHESIS_(...) ,

// WARNING: when using ISEMPTY, unlike other uses of VARIADIC, 0 is for the "not empty" case
// (e.g., 1 argument) and 1 is for the "empty" case (0 argument).
#define ISEMPTY(...)                                                    \
_ISEMPTY(                                                               \
          /* test if there is just one argument, eventually an empty    \
             one */                                                     \
          HAS_COMMA(__VA_ARGS__),                                       \
          /* test if _TRIGGER_PARENTHESIS_ together with the argument   \
             adds a comma */                                            \
          HAS_COMMA(_TRIGGER_PARENTHESIS_ __VA_ARGS__),                 \
          /* test if the argument together with a parenthesis           \
             adds a comma */                                            \
          HAS_COMMA(__VA_ARGS__ (/*empty*/)),                           \
          /* test if placing it between _TRIGGER_PARENTHESIS_ and the   \
             parenthesis adds a comma */                                \
          HAS_COMMA(_TRIGGER_PARENTHESIS_ __VA_ARGS__ (/*empty*/))      \
          )

#define PASTE5(_0, _1, _2, _3, _4) _0 ## _1 ## _2 ## _3 ## _4
#define _ISEMPTY(_0, _1, _2, _3) HAS_COMMA(PASTE5(_IS_EMPTY_CASE_, _0, _1, _2, _3))
#define _IS_EMPTY_CASE_0001 ,

#endif // ifndef VARIADIC

