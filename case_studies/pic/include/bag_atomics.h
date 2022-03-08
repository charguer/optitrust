#ifndef BAG_ATOMIC_H
#define BAG_ATOMIC_H

#include "bag.h"

chunk* atomic_read_chunk(chunk** p);
void atomic_write_chunk(chunk** p, chunk* v);
int atomic_increment(int* size);

#endif

