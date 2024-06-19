# Benchmark Suites

- PolyBench
- NAS: https://github.com/GMAP/NPB-CPP

# Physical Simulation Case Studies

- [PIC](pic).
- *FireDrake?* PDE, finite differences DSLs.
- *Ray Tracing?*
- *Ocean Modeling?*, e.g. done in Futhark
- *Room Acoustics?*, Larisa Stoltzfus paper.
- *Bionformatics?*, [Seq](https://dl.acm.org/doi/10.1145/3360551).
- *Block-Based Compression?*, "Hierarchical Multi-Dimensional Arrays and its
Implementation in the CoLa Domain Specific Language for Block-Based Data Compression"

# Image processing Case Studies

- [Box Blur](box_blur) inspired from Halide's [blur](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/blur/) app. A simple stencil.

- [Harris Corner Detection](harris) corresponding to Halide's [harris](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/harris/) app. A more interesting combination of simple stencils and pointwise filters. The algorithm used in [PolyMage](https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/harris
) is different. There's a [Rise version](https://github.com/rise-lang/shine/blob/fc3423da41fbfdf2373e007848d16936756cae77/src/main/scala/apps/harrisCornerDetectionHalide.scala). Opportunity to use circular buffer and register rotation optimizations.

- *Histogram?*, for reduction optimization patterns. Also discussed in Futhark.

- *Camera Pipeline?* corresponding to Halide's [camera_pipe](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/camera_pipe/) app. Uses fixed-width integers with interesting casts and operations, has many stencils, as well as other access patters with pixel interleavings, includes Look Up Tables. There's a [PolyMage version](https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/campipe) and a [Rise version](https://github.com/rise-lang/shine/blob/fc3423da41fbfdf2373e007848d16936756cae77/src/main/scala/apps/cameraPipeline.scala).

- *Unsharp Mask?* corresponding to Halide's [unsharp](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/unsharp/) app. Chains of stencils, including kernel weight precomputing. There's a [PolyMage version](https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/unsharp_mask) and a [Rise version](https://github.com/rise-lang/shine/blob/fc3423da41fbfdf2373e007848d16936756cae77/src/main/scala/apps/unsharpMask.scala).

- *Multiscale Interpolation?* corresponding to Halide's [interpolate](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/interpolate/) app. Includes downsampling/upsampling pyramids. There's a [PolyMage version](https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/interpolate) and a [Rise version](https://github.com/rise-lang/shine/blob/fc3423da41fbfdf2373e007848d16936756cae77/src/main/scala/apps/multiscaleInterpolation.scala).

- *Local Laplacian* corresponding to Halide's [local_laplacian](https://github.com/halide/Halide/blob/0782d80b4907f94b4bc2b0df806306952ad39111/apps/local_laplacian/) app. Includes downsampling/upsampling pyramids as well as Look Up Tables. There's a [PolyMage version](https://bitbucket.org/udayb/polymage/src/e28327c/sandbox/apps/python/img_proc/local_laplacian) and a [Rise version](https://github.com/rise-lang/shine/blob/fc3423da41fbfdf2373e007848d16936756cae77/src/main/scala/apps/localLaplacian.scala).

- *Connected Component Labeling?*, includes union find and sparse data structures, more than stencils and histograms.

- *Motion Detection?*

- *Fast Fourier Transform?*, see SPIRAL

# Deep Learning Case Studies

TVM CPU scheduling: https://tvm.d2l.ai/chapter_cpu_schedules/.
TVM GPU scheduling: https://tvm.d2l.ai/chapter_gpu_schedules/.
Includes matrix multiplication, convolution, packaed convolution, depthwise convolution, pooling, batch normalization.

- [Matrix Multiplication](matmul) corresponding to [TVM's schedule](https://tvm.apache.org/docs/how_to/optimize_operators/opt_gemm.html). Interesting variation for DL: `ReLu(MM(tanh(A), transpose(B)))`, as in this [Rise version](https://github.com/rise-lang/shine/blob/89545f3a326405d8715d704ff3bcd848515f1a4f/src/main/scala/apps/neuralNetworkPieces.scala). Transposed batched matrix multiplication. Grouped Convolutions, Multi-Layer Perception, see Tensor Comprehensions paper. Sparse MM? im2col convolution to MM transformation?

- *ResNet50?*, a 50-layer networks: 48 convolutions, one MaxPool, one average pool. Pooling uses max/avg to reduce tiles of input.

- *LeNet?*, convolution + pooling + fully connected (MM). More advanced: *AlexNet*.

- *VGG?*, deeper layers with smaller filters.

- *Transformers?*, designed to process sequential input, like RNNs (Recurrent Neural Networks).

- *ENet?*, efficient neural network, compact encoder-decoder architecture.

- Linnea linear algebra case studies?

# Graph Processing Case Studies

- *GraphIt?*

# Financial Algorithms

- See Futhark publications

# Fundamental Software Case Studies

- Optimized data structure implementations (HashMap, ..).
- Optimized sorting algorithms (e.g. [Futhark example for GPU](https://futhark-lang.org/examples/radix-sort.html)).
- Optimized binary search (e.g. [Futhark example for GPU](https://futhark-lang.org/examples/binary-search.html)).
- Parallel In-Place Algorithms: https://epubs.siam.org/doi/abs/10.1137/1.9781611976489.9
- Work-efficient scan implementations:
  - see [single pass parallel prefix scan with decoupled look back](https://research.nvidia.com/publication/2016-03_single-pass-parallel-prefix-scan-decoupled-look-back)
  - see Federico Pizzuti paper
  - sequence alignement: https://bmcbioinformatics.biomedcentral.com/articles/10.1186/s12859-016-0930-z
  - graph algorithms: https://dl.acm.org/doi/abs/10.1145/3434393
- Optimizing user code that uses library code?
  - Beyond [Weld](https://www.weld.rs/)? Weld can improve the performance of workflows such as SQL with Spark SQL, logistic regression with TensorFlow, and data cleaning in NumPy and Pandas.
