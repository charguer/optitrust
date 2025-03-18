#ifndef OPTITRUST_INTRINSICS_H
#define OPTITRUST_INTRINSICS_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <functional>

/* ---- Resource Annotations ---- */

typedef const char* __resource_list;

inline void __pure() {}
inline void __requires(__resource_list) {}
inline void __ensures(__resource_list) {}
inline void __reads(__resource_list) {}
inline void __writes(__resource_list) {}
inline void __modifies(__resource_list) {}
inline void __preserves(__resource_list) {}
inline void __consumes(__resource_list) {}
inline void __produces(__resource_list) {}

inline void __strict() {}
inline void __xrequires(__resource_list) {}
inline void __xensures(__resource_list) {}
inline void __xreads(__resource_list) {}
inline void __xwrites(__resource_list) {}
inline void __xmodifies(__resource_list) {}
inline void __xpreserves(__resource_list) {}
inline void __xconsumes(__resource_list) {}
inline void __xproduces(__resource_list) {}
inline void __srequires(__resource_list) {}
inline void __smodifies(__resource_list) {}
inline void __spreserves(__resource_list) {}
inline void __sreads(__resource_list) {}

inline void __admitted() {}

/* ---- Debug annotations ---- */

// Printing of internal representation of reified arithmetic expressions
template<typename T> T __ARITH(T t, const char* s) {
  __pure();
  __admitted();
  return t;
}

/* ---- Ghost annotations ---- */

// Return type for ghost functions
typedef void __ghost_ret;

// Type of ghost function pointers
typedef std::function<__ghost_ret()> __ghost_fn;

// Argument type for ghost functions
typedef const char* __ghost_args;
typedef const char* __ghost_bind;

// Marcro for ghost function prototype
#define __GHOST(f) inline __ghost_ret f()

// Return type of a ghost call, cannot be void because we need to be able to call ghosts at toplevel
struct __ghost_unit {};

// Invoke a ghost function
inline __ghost_unit __ghost_call(__ghost_fn, __ghost_args = "", __ghost_bind = "") { return {}; }
#define __MERGE_IDENT(a,b) a##b
#define __GHOST_CALL(id, ...) static __ghost_unit __MERGE_IDENT(__ghost__, id) = __ghost_call(__VA_ARGS__)
#define __ghost(...) __GHOST_CALL(__COUNTER__, __VA_ARGS__)

/// Postfix call for specifying ghost arguments
inline void __with(__ghost_args) {}
inline void __bind(__ghost_bind) {}
template<typename T> T __call_with(T ret_val, __ghost_args = "", __ghost_bind = "") { return ret_val; }

#endif
