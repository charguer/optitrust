// 3 major steps, following cuda_samples path
// 1. Convert to GPU kernel (Naive GPU version)
// 2. Coalesce loads using shared memory
// 3. Reduce bank conflicts.

#pragma region Naive CPU
void transpose_naive(float *A, float *B, int width, int height) {
  for (int y = 0; y < height; y++) {
    for (int x = 0; x < width; x++) {
      B[x*height + y] = A[y*width + x];
    }
  }
}
#pragma endregion


#pragma region Naive GPU

// CUDA samples code:
// Split up work into 32x32 tiles of matrix per block.
// Block size is 32x16 threads; so each thread processes 2 rows of tile.
__global__ void transpose_gpu_naive(float *B, float *A, int width, int height)
{
  int xIndex = blockIdx.x * TILE_DIM + threadIdx.x;
  int yIndex = blockIdx.y * TILE_DIM + threadIdx.y;

  int index_in  = xIndex + width * yIndex;
  int index_out = yIndex + height * xIndex;

  for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
    B[index_out + i] = A[index_in + i * width];
  }
}

// HOST SIDE
int main () {
  float *A = (float*) cudaMalloc(width * height * sizeof(float));
}
// Required to represent:
// - Thread/block hierarchy (both X and Y dimensions)
// - Different memory spaces (host and device)
//
// Could use standard tiling primitive to create block & tile loop:

#define TILE_DIM 32
#define BLOCK_ROWS 16
void transpose_gpu_naive(float *A, float *B, int width, int height) {
  for (int by = 0; by < height / TILE_DIM; by++) {    // BLOCK Y
    for (int bx = 0; bx < width / TILE_DIM; bx++) {   // BLOCK X
      for (int ty = 0; ty < BLOCK_ROWS; ty++) {       // THREAD Y
        for (int tx = 0; tx < TILE_DIM; tx++) {       // THREAD X
          int xIndex = bx * TILE_DIM + tx;
          int yIndex = by * TILE_DIM + ty;

          int index_in  = xIndex + width * yIndex;
          int index_out = yIndex + height * xIndex;
          for (int y = 0; y < 2; y++) {
            B[(index_out+(y * BLOCK_ROWS))] = A[(index_in+(y * BLOCK_ROWS))*width];
          }
        }
      }
    }
  }
}

#pragma endregion Naive GPU


#pragma region Coalesced

// The above program performs terribly because each thread in a warp accesses strided locations in memory.
// Load into shared memory first.

__global__ void transposeCoalesced(float *odata, float *idata, int width, int height)
{
  __shared__ float tile[TILE_DIM][TILE_DIM];


  int xIndex   = blockIdx.x * TILE_DIM + threadIdx.x;
  int yIndex   = blockIdx.y * TILE_DIM + threadIdx.y;
  int index_in = xIndex + (yIndex)*width;

  xIndex        = blockIdx.y * TILE_DIM + threadIdx.x;
  yIndex        = blockIdx.x * TILE_DIM + threadIdx.y;
  int index_out = xIndex + (yIndex)*height;

  MINDEX2(width, height, bx * 32 + ty*2 + j, by * 32 + tx);

  for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
    tile[threadIdx.y + i][threadIdx.x] = idata[index_in + i * width];
  }

  __syncthreads();

  for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
    odata[index_out + i * height] = tile[threadIdx.x][threadIdx.y + i];
  }
}

// Required to represent:
// - Thread/block hierarchy
// - Different memory spaces
// - Synchronization
//
// Shared memory: Can reuse binding/hoist alloc (dont know the names precisely)
// transformations to pre-load into tile.
// We can declare the shared memory in the block loop outside the thread loop.
// This seems to match the idea that shared memory is block local, though it's unclear
// if it matches CUDA semantics
//
// Thread and block loops can be thought of as parallel for.
//  Then, synchronization inside the thread loop could be interpreted as fission of these loops
//
// Code may look something like:

#define TILE_DIM 32
#define BLOCK_ROWS 16
void transpose_gpu_naive(float *A, float *B, int width, int height) {
  pfor (int by = 0; by < height / TILE_DIM; by++) {    // BLOCK Y
    pfor (int bx = 0; bx < width / TILE_DIM; bx++) {   // BLOCK X
      __shared float tile[TILE_DIM][TILE_DIM];  // <-- shared memory outside thread loop
      pfor (int ty = 0; ty < BLOCK_ROWS; ty++) {       // THREAD Y
        pfor (int tx = 0; tx < TILE_DIM; tx++) {       // THREAD X
          // body before syncthreads
        }
      }
      // would be analogous to a sync/join
      pfor (int ty = 0; ty < BLOCK_ROWS; ty++) {       // THREAD Y
        pfor (int tx = 0; tx < TILE_DIM; tx++) {       // THREAD X
          // body after syncthreads
        }
      }
    }
  }
}
// More about the limitations of doing sync this way in other kernels
// I will leave the pfor implicit from here on out

#pragma endregion Coalesced

#pragma region Bank conflicts

// Above program writes out data from tile as follows :

//              But tile width is multiple of 16 (32), so each column element hits different bank
//            /--------------\
//          / x x x x x x ...
//          | x
// warp:    | x
// 16 elems | ..
//          | x
//          \ x

//             Solution: add another column:
//              16n + 1
//            /----------------\
//          / x x x x x x ...  y
//          | x
// warp:    | x
// 16 elems | ..
//          | x
//          \ x

__global__ void transposeCoalesced(float *odata, float *idata, int width, int height)
{
  cg::thread_block cta = cg::this_thread_block();
  __shared__ float tile[TILE_DIM][TILE_DIM + 1]; // <-- Only change

  int xIndex   = blockIdx.x * TILE_DIM + threadIdx.x;
  int yIndex   = blockIdx.y * TILE_DIM + threadIdx.y;
  int index_in = xIndex + (yIndex)*width;

  xIndex        = blockIdx.y * TILE_DIM + threadIdx.x;
  yIndex        = blockIdx.x * TILE_DIM + threadIdx.y;
  int index_out = xIndex + (yInd ex)*height;

  for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
    tile[threadIdx.y + i][threadIdx.x] = idata[index_in + i * width];
  }

  __syncthreads();

  for (int i = 0; i < TILE_DIM; i += BLOCK_ROWS) {
    odata[index_out + i * height] = tile[threadIdx.x][threadIdx.y + i];
  }
}

// Required to represent:
// - Thread/block hierarchy
// - Different memory spaces
// - Synchronization
//
// Assuming the shared memory representation reuses some language constructs of
// standard memory, it should be easy to either reuse a transfo that does this
// or create one and its correctness could be easily justified by
// the programmer in the model typechecking mode.


#pragma endregion Bank conflicts
