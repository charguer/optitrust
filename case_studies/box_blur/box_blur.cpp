#include <optitrust.h>
#include "box_blur.h"

// output: (h-2).(w-8).float
// input :     h.    w.float
void blur(float* output, float* input, int w, int h) {
  int w2 = w - 8;
  int h2 = h - 2;
  float* blur_y = (float*) MALLOC2(h2, w, sizeof(float));

  // pour toutes les colonnes
  for (int x = 0; x < w; x++) {
    // pour toutes les lignes
    for (int y = 0; y < h2; y++) {
      // calcul de moyenne verticale
      blur_y[x + w*y] = (
       input[x + w*(y + 0)] +
       input[x + w*(y + 1)] +
       input[x + w*(y + 2)]
      ) / 3.0f;
    }
  }

  for (int x = 0; x < w2; x++) {
    for (int y = 0; y < h2; y++) {
      // calcul de moyenne horizontale
      output[x + w2*y] = (
       blur_y[(x + 0) + w*y] +
       blur_y[(x + 1) + w*y] +
       blur_y[(x + 2) + w*y]
      ) / 3.0f;
    }
  }
}

/*

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
