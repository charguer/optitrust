#include <stdint.h>

// Note: CUDA samples uses packed uint8_t elements in data array
// With 256 buckets, guarantees that forall i < data_len, data[i] < hist_len
// For brevity I've just made the elements uint and made the bounds check a separate concern
// I have also omitted the zeroing of the histogram first just to make life easy.
// Technically, the behavior is not preserved here because you could assume hist is initialized,
// whereas partial_hists is not, by necessity (it was malloc'd).

#pragma region Naive CPU
void hist(int *hist, int *data, int hist_len, int data_len) {
  for (int i = 0; i < data_len; i++) {
    hist[data[i]] += 1;
  }
}
#pragma endregion

#pragma region Blocks
// 1. Block/thread hierarchy:

// The cuda_samples program works by having each block maintain a partial histogram
// which corresponds to some subset of the array.
//  It accesses the dataset like this:
//    d0 d1 d2 d3 d4 ... d239  d240 d241 d242 ...
//    b0 b1 b2 b3 b4     b239  b0   b1   b2
//    \-------- i=0 --------/  \------ i=1 ------/
//
//  Then, each element is reduced to the final histogram as follows:
//   (assume 255 is the histogram length)
//  /---- hist0 ----\ /---- hist1 ----\  ...  /---- hist239 ----\
//  h0 h1 h2 ... h255 h0 h1 h2 ... d255  ...   h0 h1 h2 ... h255
//  b0 b1 b2          b0 b1 b2                 b0 b1 b2
//  |                 |                        |
//  |-------------------------------------------
//  \/
//  d0 d1 ...
//  \------------------------------ hist

#define PARTIAL_HISTOGRAMS_COUNT 240
void hist(int *hist, int *data, int hist_len, int data_len) {
  int *partial_hists = (int*)malloc(PARTIAL_HISTOGRAMS_COUNT * hist_len * sizeof(int));
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    for (int i = b; i < data_len; i += PARTIAL_HISTOGRAMS_COUNT) {
      partial_hists[hist_len * b + data[i]] += 1;
    }
  }
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    for (int i = b; i < hist_len; i += PARTIAL_HISTOGRAMS_COUNT) {
      int sum = 0;
      for (int j = 0; j < PARTIAL_HISTOGRAMS_COUNT; j++)  {
        sum += partial_hists[j*hist_len + i];
      }
      hist[i] = sum;
    }
  }
}
// Required to represent:
// None - This can all be done with existing transformations more or less
//  or at least it is all representable on CPU.
#pragma endregion

#pragma region Threads

// Break down the histogram into sub histograms again, this time per thread.

// these values will become apparent later
#define PARTIAL_HISTOGRAMS_COUNT 240
#define WARP_COUNT 6 /* Warps ==subhistograms per threadblock */
#define TPB (WARP_COUNT * 32) /* Threadblock size */
#define MERGE_THREADBLOCK_SIZE 256

void hist(int *hist, int *data, int hist_len, int data_len) {
  int *partial_hists = (int*)malloc(PARTIAL_HISTOGRAMS_COUNT * hist_len * sizeof(int));
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    int s_hist[TPB * hist_len];
    for (int t = 0; t < TPB; t++) {
      for (int i = b * TPB + t; i < data_len; i += TPB * PARTIAL_HISTOGRAMS_COUNT) {
        s_hist[hist_len * t + data[i]] += 1;
      }
    }
    for (int t = 0; t < TPB; t++) {
      for (int i = t; i < hist_len; i += TPB) {
        int sum = 0;
        for (int j = 0; j < TPB; j++) {
          sum += partial_hists[j*hist_len + i];
        }
        partial_hists[hist_len * b + i] = sum;
      }
    }
  }
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    for (int i = b; i < hist_len; i += PARTIAL_HISTOGRAMS_COUNT) {
      int sum = 0;
      for (int j = 0; j < PARTIAL_HISTOGRAMS_COUNT; j++)  {
        sum += partial_hists[j*hist_len + i];
      }
      hist[i] = sum;
    }
  }
}

// Required to represent:
// Also none (same CPU-level transformation as last time) - but there is one question:
// could it be a valid GPU program right now? Do GPU programs in OptiTrust need to have
// both a thread and a block loop, or can there be some semantics for only a block loop?
// Do we also allow back to back block loops and interpret them as different kernels?
// Do we implicitly reorder them?
// The second block loop only has a block axis still; but it may be reasonable to talk about
// what it is doing on only the block level. If back to back block loops are not allowed it could be
// fissioned into its own kernel.
// Point is though it may be easier to call this program a GPU program from here and
// transform it further.

#pragma endregion

#pragma region Merge

// Give a thread axis to the merge computation and rearrange the loops accordingly.

#define MERGE_TPB 256
#define MERGE_BLOCKS 256

void hist_split(int *partial_hists, int *data, int hist_len, int data_len) {
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    int s_hist[TPB * hist_len];
    for (int t = 0; t < TPB; t++) {
      for (int i = b * TPB + t; i < data_len; i += TPB * PARTIAL_HISTOGRAMS_COUNT) {
        s_hist[hist_len * t + data[i]] += 1;
      }
    }
    for (int t = 0; t < TPB; t++) {
      for (int i = t; i < hist_len; i += TPB) {
        int sum = 0;
        for (int j = 0; j < TPB; j++) {
          sum += partial_hists[j*hist_len + i];
        }
        partial_hists[hist_len * b + i] = sum;
      }
    }
  }
}
// More compute reordering: mostly the same
void hist_merge(int *hist, int *partial_hists, int hist_len) {
  for (int b = 0; b < MERGE_BLOCKS; b++) { // Note: this HAS to be = hist_len!
    for (int t = 0; t < MERGE_THREADBLOCK_SIZE; t++) {
      int sum = 0;
      for (int i = t; i < PARTIAL_HISTOGRAMS_COUNT; i += MERGE_THREADBLOCK_SIZE) {
        sum += partial_hists[b + i * hist_len];
      }
      hist[b] += sum;
    }
  }
}
void hist(int *hist, int *data, int hist_len, int data_len) {
  int *partial_hists = (int*)malloc(PARTIAL_HISTOGRAMS_COUNT * hist_len * sizeof(int));
  hist_split(partial_hists, data, hist_len, data_len);
  hist_merge(hist, partial_hists, hist_len);
}

// Required to represent: more of the same - compute reordering, function uninlining
// However to be correct we must encode the precondition about MERGE_BLOCKS = hist_len
#pragma endregion

#pragma region GPU safe

// The current program is valid on CPU, and has the proper thread hierarchy,
// but hist_merge has race conditions.

// Instead of
  hist[b] += sum

// which has a race condition from multiple threads in the block, the cudaSamples does

  __shared__ uint data[MERGE_THREADBLOCK_SIZE];
  data[threadIdx.x] = sum;

  for (uint stride = MERGE_THREADBLOCK_SIZE / 2; stride > 0; stride >>= 1) {
    __syncthreads();

    if (threadIdx.x < stride) {
      data[threadIdx.x] += data[threadIdx.x + stride];
    }
  }

  if (threadIdx.x == 0) {
    hist[blockIdx.x] = data[0];
  }

// 1. The sum computed by the thread is stored in shared memory indexed by TID
// 2. The shared memory is reduced to one with a standard reduction loop
// 3. Only the 0 thread writes it.

// Now the code becomes:

void hist_merge(int *hist, int *partial_hists, int hist_len) {
  for (int b = 0; b < MERGE_BLOCKS; b++) { // Note: this HAS to be = hist_len!
    __shared__ uint data[MERGE_THREADBLOCK_SIZE];
    for (int t = 0; t < MERGE_THREADBLOCK_SIZE; t++) {
      int sum = 0;
      for (int i = t; i < PARTIAL_HISTOGRAMS_COUNT; i += MERGE_THREADBLOCK_SIZE) {
        sum += partial_hists[b + i * hist_len];
      }
      data[threadIdx.x] = sum;

      for (uint stride = MERGE_THREADBLOCK_SIZE / 2; stride > 0; stride >>= 1) {
        __syncthreads();

        if (threadIdx.x < stride) {
          data[threadIdx.x] += data[threadIdx.x + stride];
        }
      }

      if (threadIdx.x == 0) {
        hist[blockIdx.x] = data[0];
      }
    }
  }
}

void hist_merge(int *hist, int *partial_hists, int hist_len) {
  for (int b = 0; b < MERGE_BLOCKS; b++) { // Note: this HAS to be = hist_len!
    __shared__ uint data[MERGE_THREADBLOCK_SIZE];
    for (int t = 0; t < MERGE_THREADBLOCK_SIZE; t++) {
      int sum = 0;
      for (int i = t; i < PARTIAL_HISTOGRAMS_COUNT; i += MERGE_THREADBLOCK_SIZE) {
        sum += partial_hists[b + i * hist_len];
      }
      data[threadIdx.x] = sum;
    }
    for (uint stride = MERGE_THREADBLOCK_SIZE / 2; stride > 0; stride >>= 1) {
      __sync(block);
      for (int t = 0; t < MERGE_THREADBLOCK_SIZE; t++) {
        if (threadIdx.x < stride) {
          data[threadIdx.x] += data[threadIdx.x + stride];
        }
      }
    }
    for (int t = 0; t < MERGE_THREADBLOCK_SIZE; t++) {
      if (threadIdx.x == 0) {
        hist[blockIdx.x] = data[0];
      }
    }
  }
}

// Same problem as reduce in terms of making this a transformation
// (do we express it as a sequential loop outside the thread loop,
//  or have transfo that introduces sync, etc.)

// For hist_split, the two thread loops were fissioned.
// Like transpose, to maintain the correctness,
// we would need to interpret this as a sync:
void hist_split(int *partial_hists, int *data, int hist_len, int data_len) {
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    int s_hist[TPB * hist_len];
    for (int t = 0; t < TPB; t++) {
      for (int i = b * TPB + t; i < data_len; i += TPB * PARTIAL_HISTOGRAMS_COUNT) {
        s_hist[hist_len * t + data[i]] += 1;
      }
      __syncthreads();
      for (int i = t; i < hist_len; i += TPB) {
        int sum = 0;
        for (int j = 0; j < TPB; j++) {
          sum += partial_hists[j*hist_len + i];
        }
        partial_hists[hist_len * b + i] = sum;
      }
    }
  }
}
// We could keep them fissioned, but splitting the loop would still need to be interpreted as a sync.

// Required to represent:
// 1. syncthreads INSIDE loop. see reduce for discussion.
// 2. shared memory
// 3. standard thread/block hierarchies.

#pragma endregion

#pragma region WARPS


// Reminder
#define PARTIAL_HISTOGRAMS_COUNT 240
#define WARP_COUNT 6 /* Warps ==subhistograms per threadblock */
#define TPB (WARP_COUNT * 32) /* Threadblock size */

// The final optimization is in hist_split. It is extremely inefficient to store a histogram for each thread,
// so instead each warp (32 threads) shares a histogram. Writes will conflict, but we use atomics to cope with that.
void hist_split(int *partial_hists, int *data, int hist_len, int data_len) {
  for (int b = 0; b < PARTIAL_HISTOGRAMS_COUNT; b++) {
    int s_hist[WARP_COUNT * hist_len];
    for (int t = 0; t < TPB; t++) {
      // warp_size = 32
      int *s_warp_hist = s_hist + (t/32) * hist_len;
      for (int i = b * TPB + t; i < data_len; i += TPB * PARTIAL_HISTOGRAMS_COUNT) {
        // was s_hist[hist_len * t + data[i]] += 1 before
        // threads in the same warp can bang on the same locations
        atomicAdd(&s_warp_hist[data[i]],1);
      }
      __syncthreads();

      for (int i = t; i < hist_len; i += TPB) {

        int sum = 0;
        // Just looping through every WARP's histogram, not every thread's anymore.
        for (int j = 0; j < WARP_COUNT; j++) {
          sum += partial_hists[j*hist_len + i];
        }
        partial_hists[hist_len * b + i] = sum;
      }
    }
  }
}

// In all, language requirements:
// - Thread, block hierarchy, different memory spaces
// - Shared memory
// - Syncing, specifically INSIDE loop per thread (reduction in hist_merge)
// - Ability to specify important assumptions about the grid size (e.g. BLOCK_SIZE = hist_size in hist_merge)
// - Atomics (in hist_split)
// - Depending on what we think is best, ability to give semantics to a program with a block loop but no thread loop?
//      It may make writing the transformations easier.
#pragma endregion
