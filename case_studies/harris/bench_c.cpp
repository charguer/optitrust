#include "bench_util.h"

extern "C" {
#include "harris.h"
}
#include "_build/harris_halide.h"
#include "halide_image_io.h"

using namespace Halide::Runtime;
using namespace Halide::Tools;

int main(int argc, char **argv) {
    if (argc != 2) {
        fprintf(stderr, "usage: %s rgba.png\n", argv[0]);
        return EXIT_FAILURE;
    }

    fprintf(stderr, "input: %s\n", argv[1]);
    Buffer<float> input = load_and_convert_image(argv[1]);

    // rotate through 10 arrays to mitigate cache interference
    const uint16_t n_allocs = 10;
    std::vector<Buffer<float>> inputs;
    inputs.push_back(input);
    for (uint16_t i = 1; i < n_allocs; i++) {
        inputs.push_back(input.copy());
    }

    std::vector<Buffer<float>> outputs;
    for (uint16_t i = 0; i < n_allocs; i++) {
        Buffer<float, 2> o(input.width() - 4, input.height() - 4);
        outputs.push_back(o);
        // outputs[i].set_min(2, 2);
    }

    uint32_t iterations = benchmark_and_print_samples([&](uint32_t i) {
        uint16_t alloc = i % n_allocs;
        harris(outputs[alloc].data(), input.height(), input.width(), inputs[alloc].data());
    });

    uint32_t to_check = std::min(iterations, (uint32_t)n_allocs);

    Buffer<float> ref_output(input.width() - 4, input.height() - 4);
    ref_output.set_min(2, 2);

    int h_error = harris_halide(input, ref_output);
    if (h_error) {
        fprintf(stderr, "halide returned an error: %d\n", h_error);
        exit(EXIT_FAILURE);
    }

    ref_output.copy_to_host();

    for (uint32_t i = 0; i < to_check; i++) {
        error_stats(ref_output.data(), outputs[i].data(), ref_output.height() * ref_output.width(), 0.01, 100);
    }

    return EXIT_SUCCESS;
}