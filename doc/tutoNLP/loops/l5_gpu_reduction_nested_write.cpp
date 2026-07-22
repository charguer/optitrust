// Difficulty: Level 5
// Request: target the write to d_partial_sums inside the bi and ti loops of reduce

void reduce(float* d_partial_sums) {
  for (int bi = 0; bi < 128; bi++) {
    for (int ti = 0; ti < 256; ti++) {
      d_partial_sums[bi] += ti;
    }
  }
}
