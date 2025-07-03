/* COPYRIGHT (c) 2014 Umut Acar, Arthur Chargueraud, and Michael
 * Rainey
 * All rights reserved.
 *
 * \file workerlocal.cpp
 * \brief Per-worker local storage
 *
 */


#include "workerlocal.hpp"

worker_id_t get_thread_id() {
  return omp_get_thread_num();
}

worker_id_t thread_id_undef = -1;

worker_id_t get_undef_thread_id() {
  return thread_id_undef;
}

worker_id_t get_thread_id_or_undef() { // undef could be 'thread_id_undef' if outside an openmp section
  return get_thread_id();
}

int get_nb_threads() {
  return omp_get_num_threads();
}
