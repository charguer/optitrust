```cpp

/* How OptiTrust implements the thread hierarchy */

Parallel for loop in optitrust:

// consume { Group(0..N, H0) } (star of H0)
parallel for (int i = 0; i < N; i++) {
  __xconsumes(H0)  <-- Exclusive ownership per-iteration
  __xproduces(H)
  __spreserves(...) <-- Invariants are not allowed!
  ...
}
// produce { Group(0..N, H) }


GPU: Use the same construct ?
like TVM, halide, etc.

parallel for (int i = 0; i < N; i++)  <-- i is a "thread ID"

__global__ void kernel(int *A) {
  int t = threadIdx.x;
  int b = blockIdx.x;
  A[b * blockDim.x + t] = 1;
}

becomes

parallel for (int b = 0; b < bpg; b++) { // blockIdx.x
  parallel for (int t = 0; t < tpb; t++) { // threadIdx.x
    d_A[b*tpb + t] = 1;
  }
}

(host side/kernel launch omitted)






X/Y axes?






__global__ void kernel(int *A) {
  int x = threadIdx.x + blockDim.x * blockIdx.x;
  int y = threadIdx.y + blockDim.y * blockIdx.y;
  A[y][x] = 1;
}

becomes

parallel for (int by = 0; b < bpg; b++) { // blockIdx.y
  parallel for (int bx = 0; b < bpg; b++) { // blockIdx.x
    parallel for (int ty = 0; ty < tpb; ty++) { // threadIdx.y
      parallel for (int tx = 0; tx < tpb; tx++) { // threadIdx.x
        d_A[by*tpb+ty][bx*tpb+tx] = 1;
      }
    }
  }
  }







Sync?


__global__ void kernel(int *A) {
  int t = threadIdx.x;
  int b = blockIdx.x;
  A[b * blockDim.x + t] = 1;
  __syncthreads();
  int t1 = (blockDim.x - t - 1);
  A[b * threadDim.x + t1] += 1;
}

becomes

parallel for (int b = 0; b < bpg; b++) {    // blocks
  parallel for (int t = 0; t < tpb; t++) {  // threads
    d_A[b*tpb + t] = 1;
  }
  // sync is a BLOCK level construct
  // use separation logic permissions to track current execution context (like `loc` in Kuiper)
  // SYNCBLOCK preserves a "block" permission
  SYNCBLOCK();
  parallel for (int t = 0; t < tpb; t++) {  // threads
    int t1 = (tpb - t - 1);
    d_A[b*tpb + t1] += 1;
  }
}




Why?
* Easier to transform from CPU code (intuitive to think about parallel for loop nest on CPU)
* One construct to support all levels of GPU hierarchy: Blocks, threads, warps, warp groups, ...
* Cooperative constructs outside the thread level.
  * => Can easily define custom constructs that expect some kind of thread cooperation.
  * As well as specifying the expected context for library/device functions.







PROBLEM: want to synchronize explicitly  (i.e. insert __syncthreads only when necessary)
existing construct is a fork & join.
E.g. this should be illegal:

parallel for (int b = 0; b < bpg; b++) { // blocks
  parallel for (int t = 0; t < tpb; t++) {
    d_A[b*tpb + t] = 1;
  }
  // Group(0..tpb, d_A[...] ~> ...)
  // Can shuffle this Group of cells freely using ghosts. Do not need to sync
  parallel for (int t = 0; t < tpb; t++) {
    int t1 = (tpb - t - 1);
    d_A[b*tpb + t1] += 1;
  }
}

OUR SOLUTION:

a special `thread for`, and a permission `DesyncGroup`

// consume { Group(0..N, H0) } (star of H0)
// consume { Threads(M) }  (I am currently executing as M threads)
thread for (int i = 0; i < N; i++) {
  // produce { Threads(M/N) } (Each iteration is M/N threads)
  __xconsumes(H0)  <-- Exclusive ownership per-iteration
  __xproduces(H)
  __spreserves(...) <-- invariants still not allowed! (same as parallel for)
  ...
}
// produce { DesyncGroup(0..N, H) }
// I can't shuffle this DesyncGroup, but I can launch another loop without syncing, as long as the access pattern is the same!

Definition of sync:

__syncthreads():
preserves GpuBlock // special "execution permission": produced by kernel launch
consumes H
produces Sync(H)

Sync(Group(.., H)) = Group(.., Sync(H))
Sync(DesyncGroup(.., H)) = Group(.., Sync(H))
Sync(p ~> v) = p ~> v


```
