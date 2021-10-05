#ifndef PIC_VERT_COMPILER_TEST
#define PIC_VERT_COMPILER_TEST

// Check for compiler version, to be able to use OpenMP simd pragmas.
// http://www.openmp.org/resources/openmp-compilers/
// OpenMP 4.0 C/C++/Fortran supported in version 15.0 and 16.0 compilers
// From GCC 4.9.1, OpenMP 4.0 is fully supported
#if defined(__clang__)                            // Clang/LLVM
#    define PIC_VERT_OPENMP_4_0
#elif defined(__ICC) || defined(__INTEL_COMPILER) // Intel ICC/ICPC
#    if (__INTEL_COMPILER_BUILD_DATE >= 20140726)
#        define PIC_VERT_OPENMP_4_0
#    endif
#elif defined(__GNUC__)                           // GCC
#    if ((__GNUC__ > 4) || (__GNUC__ == 4 && (__GNUC_MINOR__ >= 9)))
#        define PIC_VERT_OPENMP_4_0
#    endif
#elif defined(__PGI)                              // Portland Group PGCC/PGCPP
#    define PIC_VERT_OPENMP_4_0
#endif

#endif // ifndef PIC_VERT_COMPILER_TEST

