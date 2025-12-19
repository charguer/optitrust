#include <optitrust_models.h>

// TODO: GEMM alpha, beta
void mm(float* c, float* a, float* b, int m, int n, int p,
  int bm, int bn, int bk, int tm, int tn // <-- new parameters / defined in script
) {
  __requires("bm divides m, bn divides n, bk divides p, tm divides bm, tn divides bn");

  const int nblk = (m / bm) * (n / bn);
  // TODO? nblkx nblky
  const int nthr = (bm / tm) * (bn / tn);
  // TODO? (?!?) nthrx nthry

  float* const c_gmem = MALLOC2(float, m, n);
  float* const a_gmem = MALLOC2(float, m, p);
  float* const b_gmem = MALLOC2(float, p, n);

  // MATRIX2_COPY_float(c_gmem, c, m, n);
  MATRIX2_COPY_float(a_gmem, a, m, p);
  MATRIX2_COPY_float(b_gmem, b, p, n);

  for (int b; b < nblk; b++) { // par
    float a_smem[MSIZE2(bk, bm)];
    float b_smem[MSIZE2(bk, bn)];

    float rAcol[nthr][tm];
    // float* const rACol = MALLOC2(float, nthr, tm);
    // Cell(Any)(v) = Cell(v)
    float rBrow[nthr][tn];
    float rchProd[nthr][MSIZE2(tm, tn)];

    // block-level tiles
    uint32_t num_n_tiles = n / bn;
    uint32_t mrow = b / num_n_tiles;
    uint32_t mcol = b % num_n_tiles;
    const int tile_row_a = MINDEX2(m, p, mrow * bm, 0);
    const int tile_col_b = MINDEX2(p, n, 0, mcol * bn);
    const int tile_c = MINDEX2(m, n, mrow * bm, mcol * bn);

    uint32_t threadRow[nthr]; // = t / (bn / tn);
    uint32_t threadCol[nthr]; // = t % (bn / tn);

    for (int t; t < nthr; t++) { // par
      // TODO: memset
      for (uint32_t resIdxM = 0; resIdxM < tm; resIdxM++) { // thread seq
        for (uint32_t resIdxN = 0; resIdxN < tn; resIdxN++) { // thread seq
          rchProd[t][MINDEX2(tm, tn, resIdxM, resIdxN)] = 0.f;
        }
      }

      threadRow[t] = t / (bn / tn);
      threadCol[t] = t % (bn / tn);
    }

    for (uint32_t bkIdx = 0; bkIdx < p; bkIdx += bk) { // block seq
      // __syncthreads
      for (int t; t < nthr; t++) { // par
        for (uint32_t i = 0; i < bm * bk; i += nthr * 4) { // thread seq
          uint32_t row = (i + t * 4) / bk;
          uint32_t col = (i + t * 4) / bk;

          const int aVectorStart = tile_row_a + MINDEX2(m, p, row, bkIdx + col);
          for (uint32_t v = 0; v < 4; v++) { // semi-vector
            a_smem[MINDEX2(bk, bm, col + v, row)] = a_gmem[aVectorStart + v];
          }
        }
        for (uint32_t i = 0; i < bk * bn; i += nthr * 4) { // thread seq
          uint32_t row = (i + t * 4) / bn;
          uint32_t col = (i + t * 4) / bn;

          const int bVectorStart = tile_col_b + MINDEX2(p, n, bkIdx + row, col);
          for (uint32_t v = 0; v < 4; v++) { // vector
            b_smem[MINDEX2(bk, bn, row, col + v)] = b_gmem[bVectorStart + v];
          }
        }
      }

      // TODO: __syncthreads
      for (int t; t < nthr; t++) { // par
        for (uint32_t dotIdx = 0; dotIdx < bk; dotIdx++) { // thread seq
          for (uint32_t i = 0; i < tm; i++) { // thread seq
            // vecload
            rAcol[t][i] = a_smem[MINDEX2(bk, bm, dotIdx, threadRow[t] * tm + i)];
          }
          for (uint32_t i = 0; i < tn; i++) { // thread seq
            // vecload
            rBrow[t][i] = b_smem[MINDEX2(bk, bn, dotIdx, threadCol[t] * tn + i)];
          }
          for (uint32_t resIdxM = 0; resIdxM < tm; resIdxM++) { // thread seq
            for (uint32_t resIdxN = 0; resIdxN < tn; resIdxN++) { // thread seq
              rchProd[t][MINDEX2(tm, tn, resIdxM, resIdxN)] +=
                rAcol[t][resIdxM] * rBrow[t][resIdxN];
            }
          }
        }
      }
    }

    for (int t; t < nthr; t++) { // par
      for (uint32_t resIdxM = 0; resIdxM < tm; resIdxM++) { // thread seq
        for (uint32_t resIdxN = 0; resIdxN < tn; resIdxN++) { // thread seq
          const int out_index = tile_c + MINDEX2(m, n, threadRow[t] * tm + resIdxM, threadCol[t] * tn + resIdxN);
          c_gmem[out_index] = rchProd[t][MINDEX2(tm, tn, resIdxM, resIdxN)];
          // GEMM:
          // = beta * c_gmem[out_index] + alpha * rchProd[MINDEX2(tm, tn, resIdxM, resIdxN)];
        }
      }
    }
  }

  MATRIX2_COPY_float(c, c_gmem, m, n);
  // MATRIX2_COPY_float(a, a_gmem, m, p);
  // MATRIX2_COPY_float(b, b_gmem, p, n);
}
