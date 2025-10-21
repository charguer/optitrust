#ifndef OPTITRUST_GPU_H
#define OPTITRUST_GPU_H

template<typename T> T __GMEM_GET(T *p);
template<typename T> void __GMEM_SET(T *p, T v);

#endif // OPTITRUST_GPU_H
