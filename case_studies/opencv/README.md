# What is this case study?

In image processing, a blur is typically used to remove noise and smoothen images. A two-dimensional blur can be decomposed as a combination of column-based blur, row-based blur, and (optionally) the application of a normalization pass.
This case study focuses on a row-based blur
function, [as implemented in the state-of-the-art OpenCV library](https://github.com/opencv/opencv/blob/4.10.0/modules/imgproc/src/box_filter.simd.hpp#L75).

- The original OpenCV code was also copy-pasted in [box_filter_rowsum_opencv.cpp](box_filter_rowsum_opencv.cpp), and made self-contained by removing OpenCV library dependencies.
