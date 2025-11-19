#include <optitrust_models.h>

// TODO: GEMM alpha, beta
void mm(float* c, float* a, float* b, int m, int n, int p,
  int bm, int bn, int bk, int tm, int tn // <-- new parameters / defined in script
) {
  __requires("bm divides m, bn divides n, bk divides p, tm divides bm, tn divides bn");

  const int nblk = (m / bm) * (n / bn);
  const int nthr = (bm / tm) * (bn / tn);

  // TODO: allocate and copy to/from
  float* c_gmem;
  float* a_gmem;
  float* b_gmem;

  for (int b; b < nblk; b++) { // par
    float a_smem[MSIZE2(bk, bm)];
    float b_smem[MSIZE2(bk, bn)];
    for (int t; t < nthr; t++) { // par
      float rAcol[tm];
      float rBrow[tn];
      float rchProd[MSIZE2(tm, tn)];

      // TODO: memset
      for (uint32_t resIdxM = 0; resIdxM < tm; resIdxM++) {
        for (uint32_t resIdxN = 0; resIdxN < tn; resIdxN++) {
          rchProd[MINDEX2(tm, tn, resIdxM, resIdxN)] = 0.f;
        }
      }

      // block-level tiles
      uint32_t num_n_tiles = n / bn;
      uint32_t mrow = b / num_n_tiles;
      uint32_t mcol = b % num_n_tiles;
      const int tile_row_a = MINDEX2(m, p, mrow * bm, 0);
      const int tile_col_b = MINDEX2(p, n, 0, mcol * bn);
      const int tile_c = MINDEX2(m, n, mrow * bm, mcol * bn);

      uint32_t threadRow = t / (bn / tn);
      uint32_t threadCol = t % (bn / tn);

      for (uint32_t bkIdx = 0; bkIdx < p; bkIdx += bk) {
        for (uint32_t i = 0; i < bm * bk; i += nthr * 4) {
          uint32_t row = (i + t * 4) / bk;
          uint32_t col = (i + t * 4) / bk;

          const int aVectorStart = tile_row_a + MINDEX2(m, p, row, bkIdx + col);
          for (uint32_t v = 0; v < 4; v++) {
            a_smem[MINDEX2(bk, bm, col + v, row)] = a_gmem[aVectorStart + v];
          }
        }
        for (uint32_t i = 0; i < bk * bn; i += nthr * 4) {
          uint32_t row = (i + t * 4) / bn;
          uint32_t col = (i + t * 4) / bn;

          const int bVectorStart = tile_col_b + MINDEX2(p, n, bkIdx + row, col);
          for (uint32_t v = 0; v < 4; v++) {
            b_smem[MINDEX2(bk, bn, row, col + v)] = b_gmem[bVectorStart + v];
          }
        }

        for (uint32_t dotIdx = 0; dotIdx < bk; dotIdx++) {
          for (uint32_t i = 0; i < tm; i++) {
            rAcol[i] = a_smem[MINDEX2(bk, bm, dotIdx, threadRow * tm + i)];
          }
          for (uint32_t i = 0; i < tn; i++) {
            rBrow[i] = b_smem[MINDEX2(bk, bn, dotIdx, threadCol * tn + i)];
          }
          for (uint32_t resIdxM = 0; resIdxM < tm; resIdxM++) {
            for (uint32_t resIdxN = 0; resIdxN < tn; resIdxN++) {
              rchProd[MINDEX2(tm, tn, resIdxM, resIdxN)] +=
                rAcol[resIdxM] * rBrow[resIdxN];
            }
          }
        }
      }

      for (uint32_t resIdxM = 0; resIdxM < tm; resIdxM++) {
        for (uint32_t resIdxN = 0; resIdxN < tn; resIdxN++) {
          const int out_index = tile_c + MINDEX2(m, n, threadRow * tm + resIdxM, threadCol * tn + resIdxN);
          c_gmem[out_index] = rchProd[MINDEX2(tm, tn, resIdxM, resIdxN)];
          // GEMM:
          // = beta * c_gmem[out_index] + alpha * rchProd[MINDEX2(tm, tn, resIdxM, resIdxN)];
        }
      }
    }
  }
}
