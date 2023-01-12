# Usage: 
# make transfos


FLAGS := -dump-ast-details

#debug: c_ast_debug.out
#debug_enc: c_ast_debug.enc

# LATER: fix the diff tool for these tests
TESTS := \

TESTS_FOR_LATER := \
	c_address.ml \
	c_stackvar.ml \
	c_features.ml \
	include_inline.ml \

TESTS_TO_FIX := \
	c_raw.ml \
	c_serialize.ml \

TARGET_MAKE_ALL := transfo

FLAGS_MAKEFILE := -disable-clang-format

include ../Common.Makefile

transfos: $(TESTS_FOR_LATER:.ml=_out.cpp)

c_ast.cpp: c_big.cpp
	$(V)cp $< $@

c_address.cpp: c_big.cpp
	$(V)cp $< $@

c_stackvar.cpp: c_big.cpp
	$(V)cp $< $@

c_features.cpp: c_big.cpp
	$(V)cp $< $@

# LATER: use a makefile iteration to generate the above

# 'make deb' for testing the deub file
deb: c_ast_debug.out
