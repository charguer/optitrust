.PHONY: precompiled_stdlib.pch

precompiled_stdlib.pch: precompiled_stdlib.h
	clang --version
	if [ -f ../optitrust_flags.sh ]; then \
		. ../optitrust_flags.sh; \
	fi; \
	if [ "$$USE_LIBSTDCXX" = "1" ]; then \
		clang -x c++-header $^ -o $@; \
	else \
		clang -x c++-header $^ -o $@ -stdlib=libc++; \
	fi
