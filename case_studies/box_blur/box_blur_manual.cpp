
#include "box_blur.h"
#include "../../include/optitrust.h"

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
      // blur_y[x][y] =
      blur_y[x + w*y] = (
       input[x + w*(y + 0)] +
       input[x + w*(y + 1)] +
       input[x + w*(y + 2)]
      ) / 3.;
    }
  }

  for (int x = 0; x < w2; x++) {
    for (int y = 0; y < h2; y++) {
      // calcul de moyenne horizontale
      // output[x][y] =
      output[x + w2*y] = (
       blur_y[(x + 0) + w*y] +
       blur_y[(x + 1) + w*y] +
       blur_y[(x + 2) + w*y]
      ) / 3.;
    }
  }
}