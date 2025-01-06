#ifndef OPTITRUST_INTRINSICS_H
#define OPTITRUST_INTRINSICS_H

#include <stdlib.h>
#include <stdbool.h>
#include <stdint.h>
#include <assert.h>
#include <string.h>
#include <functional>

/* ---- Resource Annotations ---- */

inline void __pure() {}
inline void __requires(const char*) {}
inline void __ensures(const char*) {}
inline void __reads(const char*) {}
inline void __writes(const char*) {}
inline void __modifies(const char*) {}
inline void __consumes(const char*) {}
inline void __produces(const char*) {}

inline void __strict() {}
inline void __xrequires(const char*) {}
inline void __xensures(const char*) {}
inline void __xreads(const char*) {}
inline void __xwrites(const char*) {}
inline void __xmodifies(const char*) {}
inline void __xconsumes(const char*) {}
inline void __xproduces(const char*) {}
inline void __invariant(const char*) {}
inline void __smodifies(const char*) {}
inline void __sreads(const char*) {}

inline void __admitted() {}

/* ---- Ghost annotations ---- */

// Return type for ghost functions
typedef void __ghost_ret;

// Type of ghost function pointers
typedef std::function<__ghost_ret()> __ghost_fn;

// Argument type for ghost functions
typedef const char* __ghost_args;

// Marcro for ghost function prototype
#define __GHOST(f) inline __ghost_ret f()

// Invoke a ghost function
inline void __ghost(__ghost_fn, __ghost_args) {}

/// Postfix call for specifying ghost arguments
inline void __with(__ghost_args) {}
template<typename T> T __call_with(T ret_val, __ghost_args) { return ret_val; }

// TODO: bind et call_with
/*
template<typename T> T __bind(T ret_val, const char*) { return ret_val; }
inline void __rename(const char*) {}
*/


#endif
