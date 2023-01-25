
#include <stdlib.h>

// output: (height-2).(width-2).float
// input :     height.    width.float
void blur_fused(float* output, float* input, int width, int height) {
  for (int x = 0; x < (width - 2); x++) {
    for (int y = 0; y < (height - 2); y++) {
      float tmp[3];

      for (int k = 0; k < 3; k++) {
        tmp[k] = (
          input[x + 0 + width*(y + k)] +
          input[x + 1 + width*(y + k)] +
          input[x + 2 + width*(y + k)]
        ) / 3.0f;
      }

      output[x + (width - 2)*y] = (
       tmp[0] + tmp[1] + tmp[2]
      ) / 3.0f;
    }
  }
}

// output: (height-2).(width-2).float
// input :     height.    width.float
void blur(float* output, float* input, int width, int height) {
  float* tmp; // MALLOC2(height, width - 2, sizeof(float));

  for (int x = 0; x < (width - 2); x++) {
    for (int y = 0; y < height; y++) {
      tmp[x + (width - 2)*y] = (
       input[(x + 0) + width*y] +
       input[(x + 1) + width*y] +
       input[(x + 2) + width*y]
      ) / 3.0f;
    }
  }

  // fuse:
  // tmp[x + (width - 2)*y] ~= MINDEX2(height, width - 2, y, x)
  //  -->
  // tmp2[y] ~= MINDEX1(width - 2, y)
  //  with tmp2 : [width - 2] ~= MALLOC1(width - 2, sizeof(float))

  for (int x = 0; x < (width - 2); x++) {
    for (int y = 0; y < height; y++) {
      output[x + (width - 2)*y] = (
       tmp[x + (width - 2)*(y + 0)] +
       tmp[x + (width - 2)*(y + 1)] +
       tmp[x + (width - 2)*(y + 2)]
      ) / 3.0f;
    }
  }
}

/*
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
*/