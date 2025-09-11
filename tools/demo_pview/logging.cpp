/* COPYRIGHT (c) 2011 Umut Acar, Arthur Chargueraud, and Michael
 * Rainey
 * All rights reserved.
 *
 * \file logging.cpp
 */

#include "logging.hpp"
#include "pcmdline.hpp"

namespace pasl {
namespace util {
namespace logging {

/*---------------------------------------------------------------------*/

recorder_t the_recorder;

/*---------------------------------------------------------------------*/
// LATER: move

static inline void fwrite_double (FILE* f, double v) {
  fwrite(&v, sizeof(v), 1, f);
}

static inline void fwrite_int64 (FILE* f, int64_t v) {
  fwrite(&v, sizeof(v), 1, f);
}

/*---------------------------------------------------------------------*/


event_t::event_t() {}

void event_t::print_byte_descr(FILE* f) { }

void event_t::print_text_descr(FILE* f) { }

void event_t::record (microtime_t basetime) {
  id = (int64_t) get_thread_id();
  time = microtime::now() - basetime; // or (double) ticks::now()
// TEMP
/*
  if (microtime::seconds(time) > 0.01) {
    output ();
    atomic::die("interrupted");
  }
*/

}

void event_t::print_byte_header (FILE* f) {
  //fwrite_double (f, time);
  fwrite_int64 (f, (int64_t) time);
  fwrite_int64 (f, id);
  fwrite_int64 (f, get_type());
}

void event_t::print_byte (FILE* f) {
  print_byte_header (f);
  print_byte_descr (f);
}

void event_t::print_text_header (FILE* f) {
  fprintf(f, "%lf\t%d\t%s\t", time, (int)id, get_name().c_str());
}

void event_t::print_text (FILE* f) {
  print_text_header (f);
  print_text_descr (f);
  fprintf (f, "\n");
}

double event_t::get_time() {
  return time;
}

/*---------------------------------------------------------------------*/


recorder_t::recorder_t() {
}

recorder_t::~recorder_t() {
}


void recorder_t::add_basic_for(worker_id_t id, event_type_t type) {
  // printf("add_basic_for %ld type %d\n", id, type);
  event_t* event = new basic_event_t(type);
  event->id = id;
  event->time = microtime::now() - basetime;
  events_for[id].events.push_back(event);
}


void recorder_t::init(int nb_workers) {
  this->nb_workers = nb_workers;
  basetime = microtime::now();
  real_time = cmdline::parse_or_default_bool("log_stdout", false);
  text_mode = cmdline::parse_or_default_bool("log_text", real_time);
  set_tracking_all(false);
  bool pview = cmdline::parse_or_default_bool("pview", false); // NOTE: previously, default was false

  bool color_view = cmdline::parse_or_default_bool("color_view", false); // temporarily deprecated
  tracking[PHASES] = cmdline::parse_or_default_bool("log_phases", false);
  tracking[THREADS] = cmdline::parse_or_default_bool("log_threads", false);
  tracking[ESTIMS] = cmdline::parse_or_default_bool("log_estims", false);
  tracking[COMM] = cmdline::parse_or_default_bool("log_comm", false);
  tracking[LOCALITY] = cmdline::parse_or_default_bool("log_locality", false);
  // TEMP: accept log_estim instead of log_estims
  bool estim = cmdline::parse_or_default_bool("log_estim", false);
  if (estim)
    tracking[ESTIMS] = true;
  tracking[CSTS] = cmdline::parse_or_default_bool("log_csts", tracking[ESTIMS]);
  tracking[TRANSFER] = cmdline::parse_or_default_bool("log_transfer", false);
  tracking[STDWS] = cmdline::parse_or_default_bool("stdws", false);
  bool track_all = cmdline::parse_or_default_bool("log_all", false);
  if (track_all)
    set_tracking_all(true);
  if (color_view) {
    pview = true;
    tracking[LOCALITY] = true;
  }
  if (pview) {
    tracking[PHASES] = true;
  }
  // initialize waiting phases
  add_basic_for(0, ENTER_LAUNCH);
  for (int id = 0; id < nb_workers; id++) {
    add_basic_for(id, ENTER_WAIT);
  }
}

void recorder_t::destroy() {
}

void recorder_t::set_tracking_all(bool state) {
  for (int k = 0; k < NUM_KIND_IDS; k++)
    tracking[k] = state;
}

events_t& recorder_t::get_my_events() {
  worker_id_t id = get_thread_id_or_undef();
  assert (0 <= id && id < max_nb_workers);
  return events_for[id].events;
}

bool recorder_t::is_tracked_kind(event_kind_t kind) {
  return tracking[kind];
}

bool recorder_t::is_tracked(event_type_t type) {
  return tracking[kind_of_type(type)];
}

void recorder_t::add_nocheck(event_p event) {
  event->record(basetime);
  if (real_time) {
    atomic::acquire_print_lock();
    event->print_text(stdout);
    atomic::release_print_lock();
  }
  get_my_events().push_back (event);
}

void recorder_t::add(event_p event) {
  if (! is_tracked(event->get_type())) {
    delete event;
    return;
  } else {
    add_nocheck(event);
  }
}

static bool event_t_compare_time (event_p e1, event_p e2) {
  return (e1->get_time() < e2->get_time());
}

void recorder_t::merge_and_sort() {
  all_events.clear();
  // printf("nb thread at output : %d\n", nb_workers);
  // NOTE: not printing anything for undefined thread id, which is not supported anymore
  for (int id = 0 /*get_undef_thread_id()*/; id < nb_workers; id++) {
    events_t events = events_for[id].events;
    for (std::vector<event_p>::iterator it = events.begin(); it != events.end(); it++) {
      all_events.push_back(*it);
    }
  }
  printf("sort\n");
  std::stable_sort (all_events.begin(), all_events.end(), event_t_compare_time);
}

void recorder_t::dump_byte_to (FILE* f) {
  for (std::vector<event_p>::iterator it = all_events.begin(); it != all_events.end(); it++)
    (*it)->print_byte (f);
}

void recorder_t::dump_text_to (FILE* f) {
  for (std::vector<event_p>::iterator it = all_events.begin(); it != all_events.end(); it++)
    (*it)->print_text (f);
}

void recorder_t::dump_byte () {
  std::string fname = cmdline::parse_or_default_string ("byte_log_file", "LOG_BIN");
  FILE* f = fopen(fname.c_str(), "w");
  this->dump_byte_to (f);
  fclose (f);
}

void recorder_t::dump_text () {
  std::string fname = cmdline::parse_or_default_string ("text_log_file", "LOG");
  FILE* f = fopen(fname.c_str(), "w");
  this->dump_text_to (f);
  fclose (f);
}

void recorder_t::output () {
  // terminate waiting phases, and exit launch
  for (int id = 0; id < nb_workers; id++) {
    add_basic_for(id, EXIT_WAIT);
  }
  add_basic_for(0, EXIT_LAUNCH);

  merge_and_sort();
  dump_byte();
  if (text_mode)
    dump_text();
}

/*---------------------------------------------------------------------*/
/*
void thread_event_t::print_byte_descr(FILE* f) {
  fwrite_int64 (f, (int64_t) thread);
}

void thread_event_t::print_text_descr(FILE* f) {
  fprintf(f, "%p", thread);
}
*/
/*---------------------------------------------------------------------*/
/*/
void thread_fork_event_t::print_byte_descr(FILE* f) {
  fwrite_int64 (f, (int64_t) thread);
  fwrite_int64 (f, (int64_t) threadL);
  fwrite_int64 (f, (int64_t) threadR);
}

void thread_fork_event_t::print_text_descr(FILE* f) {
  fprintf(f, "%p\t%p\t%p", thread, threadL, threadR);
}
*/
/*---------------------------------------------------------------------*/

void interrupt_event_t::print_byte_descr(FILE* f) {
  fwrite_double (f, elapsed);
}
void interrupt_event_t::print_text_descr(FILE* f) {
  fprintf(f,"%lf\t", elapsed);
}

/*---------------------------------------------------------------------*/

void locality_event_t::print_byte_descr(FILE* f) {
  //! \todo only works if thread::locality_t is int64_t
  fwrite_int64 (f, (int64_t) pos);
}

void locality_event_t::print_text_descr(FILE* f) {
  fprintf(f, "%ld", pos);
}

/*---------------------------------------------------------------------*/

void estim_name_event_t::print_byte_descr(FILE* f) {
  fwrite_int64 (f, (int64_t) estim);
  int64_t len = (int64_t) name.length();
  fwrite_int64 (f, len);
  for (int64_t i = 0; i < len; i++)
    fwrite_int64 (f, (int64_t) name[i]);
}

void estim_name_event_t::print_text_descr(FILE* f) {
  fprintf(f,"%p\t%s\t", estim, name.c_str());
}

void estim_report_event_t::print_byte_descr(FILE* f) {
  fwrite_int64 (f, (int64_t) estim);
  fwrite_int64 (f, (int64_t) comp);
  // TODO: fix double bits: fwrite_double (f, elapsed);
  fwrite_int64 (f, (int64_t) (1000.0 * elapsed));
  fwrite_double (f, newcst);
}

void estim_report_event_t::print_text_descr(FILE* f) {
  // warning: order switched
  fprintf(f,"%p\t%ld\t%lf\t%lf\t", estim, (long)comp, newcst, elapsed);
}

void estim_update_event_t::print_byte_descr(FILE* f) {
  fwrite_int64 (f, (int64_t) estim);
  fwrite_double (f, newcst);
}
void estim_update_event_t::print_text_descr(FILE* f) {
  fprintf(f,"%p\t%lf\t", estim, newcst);
}

void estim_predict_event_t::print_byte_descr(FILE* f) {
  fwrite_int64 (f, (int64_t) estim);
  fwrite_int64 (f, (int64_t) comp);
  fwrite_double (f, time);
}
void estim_predict_event_t::print_text_descr(FILE* f) {
  // warning: extra info printed
  double cst = time / comp;
  fprintf(f,"%p\t%ld\t                     \t%lf\t%lf\t", estim, (long)comp, cst, time);
}

/*---------------------------------------------------------------------*/

void output () {
  the_recorder.output();
}

bool is_tracked_kind(event_kind_t kind) {
  return the_recorder.is_tracked_kind(kind);
}

void log_event(event_p event) {
  the_recorder.add(event);
}

void log_basic(event_type_t type) {
  if (! the_recorder.is_tracked(type))
    return;
  // I don't think we want to be calling "new" here; it'd be more efficient
  // to push the raw event value on the event buffer -- Mike
  the_recorder.add_nocheck(new basic_event_t(type));
}
/*
void log_thread(event_type_t type, sched::thread_p thread) {
  if (! the_recorder.is_tracked(type))
    return;
  the_recorder.add_nocheck(new thread_event_t(type, thread));
}

void log_thread_fork(event_type_t type, sched::thread_p thread, sched::thread_p threadL, sched::thread_p threadR) {
  if (! the_recorder.is_tracked(type))
    return;
  the_recorder.add_nocheck(new thread_fork_event_t(type, thread, threadL, threadR));
}
*/


/*---------------------------------------------------------------------*/


} // end namespace
} // end namespace
} // end namespace
