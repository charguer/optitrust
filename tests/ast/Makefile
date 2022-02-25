
FLAGS := -dump-ast-details

#debug: c_ast_debug.out
#debug_enc: c_ast_debug.enc

TESTS := \
	c_address.ml \
	c_stackvar.ml \
	include_inline.ml \

TESTS_TO_FIX := \
	c_features.ml \
	c_ast.ml \
	c_serialize.ml \
	c_compound_assign.ml \

# TODO: revive "encodings.ml" ?

# TODO: use the ~filename feature in the test to share c_big.cpp or c_ast.cpp
# instead of copy pasting its contents in c_ast.cpp and c_serialize.cpp etc...


#	serialize.ml
#   rawc.ml

TARGET_MAKE_ALL := transfo

include ../Common.Makefile
