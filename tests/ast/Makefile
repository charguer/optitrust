
FLAGS := -dump-ast-details

#debug: c_ast_debug.out
#debug_enc: c_ast_debug.enc

TESTS := \
	c_ast.ml \
	c_address.ml \
	c_stackvar.ml \
	c_features.ml \
	include_inline.ml \

TESTS_TO_FIX := \
	c_raw.ml \
	c_serialize.ml \

TARGET_MAKE_ALL := transfo

include ../Common.Makefile


c_ast.cpp: c_big.cpp
	$(V)cp $< $@

c_address.cpp: c_big.cpp
	$(V)cp $< $@

c_stackvar.cpp: c_big.cpp
	$(V)cp $< $@

c_features.cpp: c_big.cpp
	$(V)cp $< $@

# LATER: use a makefile iteration to generate the above
