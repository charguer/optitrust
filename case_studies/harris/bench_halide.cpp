#include "bench_util.cpp" // TODO: header?

#include "_build/harris_halide.h"
#include "halide_image_io.h"

using namespace Halide::Runtime;
using namespace Halide::Tools;

int main(int argc, char **argv) {
    if (argc != 3) {
        fprintf(stderr, "usage: %s rgba.png output.png\n", argv[0]);
        return EXIT_FAILURE;
    }

    fprintf(stderr, "input: %s\n", argv[1]);
    Buffer<float> input = load_and_convert_image(argv[1]);
    for (uint16_t i = 0; i < input.dimensions(); i++) {
        fprintf(stderr, "    %d from %d by %d\n", input.dim(i).extent(), input.dim(i).min(), input.dim(i).stride());
    }

    const char* output_path = argv[2];

    // rotate through 10 arrays to mitigate cache interference
    const uint16_t n_allocs = 10;
    std::vector<Buffer<float>> inputs;
    inputs.push_back(input);
    for (uint16_t i = 1; i < n_allocs; i++) {
        inputs.push_back(input.copy());
    }

    std::vector<Buffer<float>> outputs;
    for (uint16_t i = 0; i < n_allocs; i++) {
        Buffer<float, 2> o(input.width() - 6, input.height() - 6);
        outputs.push_back(o);
        outputs[i].set_min(3, 3);
    }

    uint32_t iterations = benchmark_and_print_samples([&](uint32_t i) {
        uint16_t alloc = i % n_allocs;
        int h_error = harris(inputs[alloc], outputs[alloc]);
        outputs[alloc].device_sync();

        if (h_error) {
            fprintf(stderr, "halide returned an error: %d\n", h_error);
            exit(EXIT_FAILURE);
        }
    });

    uint32_t to_check = std::min(iterations, (uint32_t)n_allocs);

    outputs[0].copy_to_host();
    fprintf(stderr, "output: %s\n", output_path);
    for (uint16_t i = 0; i < outputs[0].dimensions(); i++) {
        fprintf(stderr, "    %d from %d by %d\n", outputs[0].dim(i).extent(), outputs[0].dim(i).min(), outputs[0].dim(i).stride());
    }

    for (uint32_t i = 1; i < to_check; i++) {
        outputs[i].copy_to_host();
        error_stats(outputs[0].data(), outputs[i].data(), outputs[0].height() * outputs[0].width(), 0.01, 100);
    }

    // TODO: benchmark
    // #include "harris_auto_schedule.h"
    // harris_auto_schedule(input, output2);
    // ?

    convert_and_save_image(outputs[0], output_path);
    return EXIT_SUCCESS;
}