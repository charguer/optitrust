
.PHONY: precompiled_stdlib.pch

# Forcing use of Clang's stdlib
precompiled_stdlib.pch: precompiled_stdlib.h
	clang -x c++-header $^ -o $@ -stdlib=libc++

# -stdlib=libc++
#  -stdlib=libc++
