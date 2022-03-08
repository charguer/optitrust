#ifndef MYMACROS_H
#define MYMACROS_H

// #define TRACE printf
#define TRACE

#ifdef CHECKER
#define CHECKER_ONLY(X) X
#define CHECKER_ONLY_COMMA(X) X,
#define STR(a) STRINTERNAL(a)
#define STRINTERNAL(a) #a
#define CHECKER_FILENAME STR(CHECKER)
#else
#define CHECKER_ONLY(X)
#define CHECKER_ONLY_COMMA(X)
#endif

#endif
