
#include <stdlib.h>

// output: (height-2).(width-2).float
// input :     height.    width.float
void blur(float* output, float* input, int width, int height) {
  float* tmp; //[(width - 2) * height];

  for (int x = 0; x < (width - 1); x++) {
    for (int y = 0; y < height; y++) {
      tmp[x + (width - 2)*y] = (
       input[x - 1 + width*y] +
       input[x - 0 + width*y] +
       input[x + 1 + width*y]
      ) / 3.0f;
    }
  }

  for (int x = 0; x < (width - 2); x++) {
    for (int y = 0; y < height; y++) {
      output[x + (width - 2)*y] = (
       input[x + (width - 2)*(y - 1)] +
       input[x + (width - 2)*(y + 0)] +
       input[x + (width - 2)*(y + 1)]
      ) / 3.0f;
    }
  }
}

int main() {
  const int W = 1024;
  const int H = 1024;
  
  float* output = (float*) calloc((W-2) * (H-2), sizeof(float));
  float* input = (float*) malloc(W * H * sizeof(float));

  // TODO: init?

  blur(output, input, W, H);

  // TODO: check result?

  free(output);
  free(input);

  return 0;
}