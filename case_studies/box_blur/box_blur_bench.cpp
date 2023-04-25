#include <cassert>
#include <cstdio>
#include <cstdlib>

#include "HalideBuffer.h"
#include "HalideRuntime.h"

#include "box_blur.h"
#include "halide_blur.h"

#include "halide_benchmark.h"
#include "halide_image_io.h"

using namespace Halide::Runtime;
using namespace Halide::Tools;

void blur_halide(Buffer<float, 2> input, Buffer<float, 2> output) {
    halide_blur(input, output);
}

void blur_optitrust(Buffer<float, 2> input, Buffer<float, 2> output) {
    blur(output.data(), input.data(), input.width(), input.height());
}

void benchmark_n_blurs(Buffer<float, 2> input, const int n,
                       const char* name,
                       void (*blur_f)(Buffer<float, 2>, Buffer<float, 2>)) {
    Buffer<float, 2> steps[n + 1];
    steps[0] = input;
    for (int i = 0; i < n; i++) {
        // width - 8 instead of width - 2, for easy 8-wise vectorization
        steps[i+1] = Buffer<float, 2>(steps[i].width() - 8, steps[i].height() - 2);
    }
    
    double best = benchmark([&]() {
        for (int i = 0; i < n; i++) {
          blur_f(steps[i], steps[i+1]);
        }
        steps[n].device_sync();
    });
    
    printf("%s runtime: %.1fms\n", name, best * 1e3);
    printf("%s fps: %.0f\n", name, floor(1.0 / best));

    convert_and_save_image(steps[n], (std::string)"images/blurred_" + name + ".png");
}

int main(int argc, char** argv) {
    if (argc != 3) {
        printf("usage: %s variant input\n", argv[0]);
        return 1;
    }

    std::string variant = argv[1];
    Buffer<float, 2> input = load_and_convert_image((std::string)"images/" + argv[2]);
    assert(input.width() % 8 == 0);

    if (variant == "reference") {
        benchmark_n_blurs(input, 10, "reference", blur_halide);
    } else {
        // if (variant == "OptiTrust") or other
        benchmark_n_blurs(input, 10, variant.c_str(), blur_optitrust);
    }

    printf("done!\n");
    return 0;
}