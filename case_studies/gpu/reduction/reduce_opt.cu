// The cuda_studies program reduces multiple chunks of an array in parallel.
// I think they just did this so they could have the blocks each doing something different.
// Regardless, it changes the spec of the function, so I am going with it.
// N = length of arr
// M = length of out
// TPB = chunk of the array N that corresponds to one element in M (basically threads per block)
// TPB * M is allowed to run past the length of N. In this case, that chunk is simply truncated.
//
// It's unfortunately very GPU specific, I can't think of a way to get around it.

#pragma region Naive CPU
void reduce(float *arr, float *out, int N, int M, int TPB) {
  for (int i = 0; i < M; i++) {
    float sum = 0.f;
    //                     ￬ truncation behavior
    for (int j = 0; j < N - TPB * i; j++) {
      sum += arr[j + (i * TPB)];
    }
    out[i] = sum;
  }
}
// Could start with a single value naive cpu reduction if you do log(n) kernel launches

#pragma endregion

#pragma region Shmem
// Note in the above program, the block and thread axes already exist! (i, j)
// we rename them here. For it to be a GPU program, we have to fix the race condition
// on `sum`. We first introduce a shared memory buffer for temporary results,
// and we change the truncation behavior to simply add 0.

void reduce(float *arr, float *out, int N, int M, int TPB) {
  for (int b = 0; b < M; b++) {
    float sdata[TPB];
    float sum = 0.f;
    for (int t = 0; t < TPB; t++) {
      int i = t + (b * TPB);
      sdata[t] = (i < N) ? arr[i] : 0.;
    }
    for (int t = 0; t < (TPB); t++) {
      sum += sdata[t];
    }
    out[b] = sum;
  }
}

// Required to represent:
// all possible to represent in CPU.
// this is technically in "CPU space" so the shared memory doesnt really mean anything,
// it's just a buffer.
#pragma endregion

#pragma region Naive GPU

// The naive GPU version of cuda_samples does this:
  for (unsigned int s = 1; s < blockDim.x; s *= 2) {
    if ((tid % (2 * s)) == 0) {
      sdata[tid] += sdata[tid + s];
    }

  }

  if (tid == 0) {
    out[blockIdx.x] = sdata[0];
  }

// It is possible to express this in CPU by putting the "for (unsigned int s ...)"
// outside the thread loop. But it cannot be inside, because with sequential for semantics
// this tree reduction does not work.
// Possible choices:
// 1. make the standard way to express this a for loop outside the thread loop, even on GPU -
//    the outer loop is simply interpreted as a sync
//
// 2. #1 except there is a transformation that can turn it into the __sync form when going to GPU
//    Requires that sync exists in language of course
//
// 3. However this loop is normally introduced, make it so that you can introduce it directly
// as a parallel for with a sync inside the loop?

// Pros of 1, don't need to add anything to the language. Cons, it may be limited.
// I think you would run into trouble with scoping.
// in CUDA, you are allowed to put syncthreads anywhere, and you may put it in a context
// that requires the value of something in the scope of the innermost loop which is the thread.
// e.g.
//
//    __shared stuff[TPB];
//    stuff[(t + TPB/2) % TPB] = ...;
//    for (int i = 0; i < stuff[t]; i++)  {
//        ... // loop body
//       __syncthreads();
//    }
//
// this is a pathological code but it cannot be meaningfully hoisted out of the thread loop.

// for now I am assuming the transformation can be done somehow and not breaking it down further.

void reduce(float *arr, float *out, int N, int M, int TPB) {
  for (int b = 0; b < M; b++) { // BLOCKS
    __shared float sdata[TPB];
    for (int t = 0; t < TPB; t++) { // THREADS
      int i = t + (b * TPB);
      sdata[t] = (i < N) ? arr[i] : 0.;
    }
    // fission as __sync once again
    for (int t = 0; t < TPB; t++) {
      for (unsigned int s = 1; s < TPB; s *= 2) {
        if ((t % (2 * s)) == 0) {
          sdata[t] += sdata[t + s];
        }

        __syncthreads();
      }

      if (t == 0) {
        out[b] = sdata[0];
      }
    }
  }
}

// Required to represent:
// 1. __syncthreads() INSIDE sequential loop in thread loop.
// 2. Thread, block hierarchies, shared memory.

#pragma endregion

#pragma region Accesses

// There are several versions of the reduce kernel in cuda_samples inbetween the last and the next
// but as far as I can tell they are just rearranging the access patterns to
// avoid bank conflicts & other memory accesses that destroy performance.

void reduce(float *arr, float *out, int N, int M, int TPB) {
  for (int b = 0; b < M; b++) { // BLOCKS
    __shared float sdata[TPB];
    for (int t = 0; t < TPB; t++) { // THREADS
      int i = t + (b * TPB);
      sdata[t] = (i < N) ? arr[i] : 0.;
    }
    for (int t = 0; t < TPB; t++) {
      for (unsigned int s = TPB / 2; s > 0; s >>= 1) {
        if (t < s) {
            sdata[t] += sdata[t + s];
        }

        __syncthreads();
      }

      if (t == 0) {
        out[b] = sdata[0];
      }
    }
  }
}

// not GPU specific stuff.

#pragma endregion

#pragma region Peel

// The next step is to do the first level of reduction in global memory,
// when sdata is being written to:

void reduce(float *arr, float *out, int N, int M, int TPB) {
  for (int b = 0; b < M; b++) { // BLOCKS
    __shared float sdata[TPB / 2];
    for (int t = 0; t < TPB / 2; t++) { // THREADS
      int i = t + (b * TPB) * 2; // now only the even chunks of arr are read
      float sum = (i < N) ? arr[i] : 0.;
      if (i + TPB < N) {
        sum += arr[i + TPB];
      }
      sdata[t] = sum;
    }
    for (int t = 0; t < TPB / 2; t++) {
      for (unsigned int s = TPB / 4; s > 0; s >>= 1) {
        if (t < s) {
            sdata[t] += sdata[t + s];
        }

        __syncthreads();
      }

      if (t == 0) {
        out[b] = sdata[0];
      }
    }
  }
}

// I think this may be more complicated to reason about.
// In the original version, sdata contains simply arr[MINDEX2(M, TPB, b, i)];
// In this new version, it contains
//    arr[MINDEX2(M, TPB/2, 2*b, i) + MINDEX2(M, TPB/2, 2*b+1, i)]  (I think)
// That amounts to the same thing as before once the reduction happens on sdata,
// and it is just associating the computation, so it should be correct.
// But, in order to maintain the same functionality, it must be called with half the
// threads, and half the blocks.
//
// Of course, it is not mentioned in the reduction.cu that calling this kernel
//  with the same threads per block results in something different.. but it looks
//  like they definitely change the calling parameters on the host side:
//
/*
  if (whichKernel < 3) { // <---- 3 is this kernel
      threads = (n < maxThreads) ? nextPow2(n) : maxThreads;
      blocks  = (n + threads - 1) / threads;
  }
  else {
      threads = (n < maxThreads * 2) ? nextPow2((n + 1) / 2) : maxThreads;
      blocks  = (n + (threads * 2 - 1)) / (threads * 2);
  }
*/
// In any case, the sdata[] that is produced by the first loop is not the same,
// so it has to be reasoned about globally between both parallel loops in some sense.

#pragma endregion


#pragma region Shuffle

// Now the reduction through shared memory is only done to the extent of a warp.

void reduce(float *arr, float *out, int N, int M, int TPB) {
  for (int b = 0; b < M; b++) { // BLOCKS
    __shared float sdata[TPB / 2];
    for (int t = 0; t < TPB / 2; t++) { // THREADS
      int i = t + (b * TPB) * 2; // now only the even chunks of arr are read
      float sum = (i < N) ? arr[i] : 0.;
      if (i + TPB < N) {
        sum += arr[i + TPB];
      }
      sdata[t] = sum;
    }
    for (int t = 0; t < TPB / 2; t++) {
      for (unsigned int s = TPB / 4; s > 32; s >>= 1) {
        if (t < s) {
            sdata[t] += sdata[t + s];
        }

        __syncthreads();
      }

      // Now only the first 32 elements of sdata are relevant. We reduce this further
      if (t < warpSize) {
        float sum = sdata[t];
        for (unsigned int s = warpSize / 2; s > 0; s >>= 1) {
          // 1st argument is a mask, selecting all threads
          sum += __shfl_down_sync(0xFFFFFFFF, sum, s);
        }
      }

      if (t == 0) {
        out[b] = sdata[0];
      }
    }
  }
}

// This could be accomplished by loop (splitting?? not fission) on the sdata loop,
// but then we would need to somehow do the transform into a warp shuffle.
// Unclear how to even give the shuffle semantics at the moment..

// Required to represent: (in total):
// - Block, thread hierarchies
// - Shared memory
// - Syncthreads inside loop
// - Warp shuffle

#pragma endregion


// TODO: the rest of the optimizations they make, I have not had the time to review.
