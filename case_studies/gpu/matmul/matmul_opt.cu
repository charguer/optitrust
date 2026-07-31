// Code adapted from https://github.com/siboehm/SGEMM_CUDA/blob/5a7dcc513d951ba764d51bc9d587b3163f3a894d/src/kernels/6_kernel_vectorize.cuh

#include <iostream>
#include <cstdint>
// #include <cuda_runtime.h>

//// BEGIN TUNING PARAMS
// BM divides rows
#define BM 32
// BN divides cols
#define BN 32
// BK divides shared
#define BK 16
// TM divides BM
#define TM 8
// TN divides BN
#define TN 8
//// END TUNING PARAMS

#define GRID_DIM(rows, cols) ((rows)/BM * ((cols)/BN))
// COMMENT: I would usually use blockDim.x,
//          but this may be helping the compiler more
#define BLOCK_DIM (BM/TM * (BN/TN))


__global__
void blocktiling2d(
    float_t alpha,
    float_t beta,
    uint32_t shared,
    uint32_t cols,
    float_t *gA,
    float_t *gB,
    float_t *gC
) {
    // shared memory cache
    // many hand written implementations would use:
    //  __shared__ float_t sA[BM*BK];
    //  __shared__ float_t sB[BK*BN];
    // but this leads to less flexibility in allocating shared memory
    // instead use dynamic shared memory with
    //   size in bytes == (BM*BK + BK*BN) * (sizeof(float_t))
    extern __shared__ float_t buffer[];
    float_t *sA = buffer;
    float_t *sB = &buffer[BM * BK];

    // thread local cache
    float_t rAcol[TM];
    float_t rBrow[TN];
    float_t rchProd[TM][TN];

    uint32_t num_n_tiles = cols / BN;
    // for each block, jump to beginning of tile row in gA
    uint32_t mrow = blockIdx.x / num_n_tiles;
    gA += shared * (mrow * BM);
    // for each block, jump to beginning of tile column in gB
    uint32_t mcol = blockIdx.x % num_n_tiles;
    gB += mcol * BN;
    // for each block, jump to the beginnin of the result tile in gC
    gC += mrow * BM * cols + mcol * BN;

    // row and column for each thread tile within the block tile
    uint32_t threadRow = threadIdx.x / (BN / TN);
    uint32_t threadCol = threadIdx.x % (BN / TN);

    for (uint32_t bkIdx = 0; bkIdx < shared; bkIdx += BK) {
        __syncthreads();
        // load A vectorized and transposed into shared memory
        for (uint32_t i = 0; i < BM * BK; i += BLOCK_DIM * 4) {
            uint32_t row = (i + threadIdx.x * 4) / BK;
            uint32_t col = (i + threadIdx.x * 4) % BK;

            // COMMENT: Instead of using bkIdx in the indexing, we could make sure
            //   to advance gA and gB to the next blockTile every iteration.
            //     gA += BK;
            //     gB += BK * cols;
            //   In that case, bkIdx is never used anywhere. I find this unintuitive.
            float4 tmp = reinterpret_cast<float4*>(&gA[bkIdx + shared * row + col])[0];
            sA[(col + 0) * BM + row] = tmp.x;
            sA[(col + 1) * BM + row] = tmp.y;
            sA[(col + 2) * BM + row] = tmp.z;
            sA[(col + 3) * BM + row] = tmp.w;
        }
        // load B vectorized into shared memory
        for (uint32_t i = 0; i < BK * BN; i += BLOCK_DIM * 4) {
            uint32_t row = (i + threadIdx.x * 4) / BN;
            uint32_t col = (i + threadIdx.x * 4) % BN;

            reinterpret_cast<float4*>(&sB[row * BN + col])[0] =
              reinterpret_cast<float4*>(&gB[cols * bkIdx + cols * row + col])[0];
        }
        __syncthreads();

        // compute subproducts per thread tile
        for (uint32_t dotIdx = 0; dotIdx < BK; dotIdx++) {
            for (uint32_t i = 0; i < TM; i++) {
                rAcol[i] = sA[dotIdx * BM + threadRow * TM + i];
            }
            for (uint32_t i = 0; i < TN; i++) {
                rBrow[i] = sB[dotIdx * BN + threadCol * TN + i];
            }
            for (uint32_t resIdxM = 0; resIdxM < TM; resIdxM++) {
                for (uint32_t resIdxN = 0; resIdxN < TN; resIdxN++)
                    rchProd[resIdxM][resIdxN] +=
                        rAcol[resIdxM] * rBrow[resIdxN];
            }
        }
    }

    // scale results of gA*gB, add scaled values from gC and write back into gC
    for (uint32_t resIdxM = 0; resIdxM < TM; resIdxM++) {
        for (uint32_t resIdxN = 0; resIdxN < TN; resIdxN++) {
            float_t *outElem =
              &gC[(threadRow * TM + resIdxM) * cols + threadCol * TN + resIdxN];
            outElem[0] = beta * outElem[0] + alpha * rchProd[resIdxM][resIdxN];
        }
    }
}

void check_error(cudaError_t err, const char *file, int line)
{
	if (err != cudaSuccess) {
        std::cerr << "ERROR -- " << file << ":" << "line" << std::endl;
        std::cerr << "Reason: " << cudaGetErrorString(err);
		exit(1);
	}
}
#define ERROR(err) check_error(err, __FILE__, __LINE__)

void
blocktiling2d_host(
    float_t alpha,
    float_t beta,
    uint32_t rows,
    uint32_t shared,
    uint32_t cols,
    float_t *gA,
    float_t *gB,
    float_t *gC
) {
    uint32_t nblk = GRID_DIM(rows, cols);
    uint32_t nthr = BLOCK_DIM;
    ERROR(cudaFuncSetAttribute(blocktiling2d,
                              cudaFuncAttributeMaxDynamicSharedMemorySize,
                              (BM*BK + BK*BN) * sizeof(float_t)));
    blocktiling2d<<<nblk, nthr, (BM*BK + BK*BN) * sizeof(float_t)>>>(alpha, beta, shared, cols, gA, gB, gC);
    ERROR(cudaDeviceSynchronize());
}
