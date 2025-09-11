#ifndef FORWARD_H
#define FORWARD_H
#include <stddef.h>
#include "types.h"


void generate_prompt_proc(transformer_t *transformer_t, int * sequence, int sequence_len);
void softmax(int col_count, int col_stride, float *x) ;
void driver(transformer_t *transformer, int token, int pos,
              int logits_count);
#endif
