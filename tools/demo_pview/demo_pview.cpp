// Demo for the pview tool, see README.md

#include "logging.hpp"
#include "pcmdline.hpp"
#include <stdio.h>

int fib(int i) {
  if (i <= 1)
    return 1;
  return fib(i-1) + fib(i-2);
}

int main(int argc, char** argv) {
  bool report = false;
  bool text_mode = true;


  pasl::util::cmdline::set(argc, argv);
  int nb_threads = (pasl::util::cmdline::parse_or_default_int("proc", 1, true));
  printf("START with %d threads\n", nb_threads);

  omp_set_num_threads(nb_threads);

  // using namespace pasl::util::logging;
  LOG_ONLY(pasl::util::logging::the_recorder.init(nb_threads));
  LOG_ONLY(pasl::util::logging::the_recorder.set_tracking_all(true));

  // generate also LOG and not just LOG_BIN
  if (text_mode) {
    LOG_ONLY(pasl::util::logging::the_recorder.text_mode = true);
  }

  if (false) { // real time debugging, on stdout
    LOG_ONLY(pasl::util::logging::the_recorder.real_time = true);
  }

  //int64_t id = (int64_t) get_thread_id();
  //printf("id=%ld\n", id);
  // if (true) return 0;

  // general init operations here
  LOG_BASIC(ENTER_ALGO);
  // -- start algo first phase

  int64_t result = 0;
  // Create 1000 tasks that accumulate in 'result'
  #pragma omp parallel for
  for (int i = 0; i < 100; i++) {
    worker_id_t thread = get_thread_id();
    if (report)
      printf("Executing task %d on thread %ld out of %d threads\n", i, thread, get_nb_threads());
    LOG_BASIC(EXIT_WAIT);
    if (report) {
      printf("Executing starts %ld\n", pasl::util::microtime::now());
    }
    // Begin contents of a task
    int r = 0;
    // for (int j = 0; j < 100; j++) {
    //   r += i;
    // }
    r += fib(32);
    #pragma omp atomic
      result += r;
    // End of contents of a task
    if (report) {
      printf("Execution stops %ld\n", pasl::util::microtime::now());
    }
    LOG_BASIC(ENTER_WAIT);
    if (report)
      printf("End task %d\n", i);
  }

  printf("result %ld\n", result);

  // If multiple phases, use this: LOG_BASIC(ALGO_PHASE);

  // -- end algo
  LOG_BASIC(EXIT_ALGO);
  // general free operations here

  // dump
  // LOG_ONLY(printf("Logging activated\n"));
  LOG_ONLY(pasl::util::logging::output());
  LOG_ONLY(pasl::util::logging::the_recorder.destroy());
  LOG_ONLY(printf("LOG_BIN generated\n"));
  if (text_mode) {
    LOG_ONLY(printf("LOG generated\n"));
  }
  return 0;
}



