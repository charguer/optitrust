// Demo for the pview tool, see README.md

#include "logging.hpp"
#include "pcmdline.hpp"
#include <stdio.h>


int main(int argc, char** argv) {
  printf("START\n");
  pasl::util::cmdline::set(argc, argv);
  printf("run\n");
   using namespace pasl::util::logging;

  LOG_ONLY(the_recorder.init());
  LOG_BASIC(ENTER_LAUNCH);
  // #pragma omp parallel num_threads(4)
  // int nb_threads = get_nb_threads();
  // printf("nb_threads %d\n", nb_threads);

  // general init operations here
  LOG_BASIC(ENTER_ALGO);
  // -- start algo first phase

  int64_t result = 0;
  // Create 1000 tasks that accumulate in 'result'
  #pragma omp parallel for
  for (int i = 0; i < 1000; i++) {
  printf("X\n");
    worker_id_t thread = get_thread_id();
    LOG_BASIC(EXIT_WAIT);
    // Begin contents of a task
    int r = 0;
    for (int j = 0; j < 1000; j++) {
      r += i;
    }
    #pragma omp atomic
      result += r;
    // End of contents of a task
    LOG_BASIC(ENTER_WAIT);
  }

  printf("result %ld\n", result);

  // If multiple phases, use this: LOG_BASIC(ALGO_PHASE);

  // -- end algo
  LOG_BASIC(EXIT_ALGO);
  // general free operations here
  LOG_BASIC(EXIT_LAUNCH);

  // dump
  LOG_ONLY(printf("Logging activated\n"));

  LOG_ONLY(output());
  LOG_ONLY(printf("LOG_BIN generated\n"));
  return 0;
}



