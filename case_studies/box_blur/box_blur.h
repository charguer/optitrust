#include <cstdint>

// output: (height-8).(width-2).float
//          width - 8 instead of width - 2, for easy 8-wise vectorization
// input :     height.    width.float
void blur(float* output, float* input, int width, int height);