int fetch_and_add(int *p) {
  int old;
#pragma omp atomic capture
  {
    old = (*p);
    (*p)++;
  }
  return old;
}

typedef struct {
  int ticketnumber;
  int turn;
} locktype;

void do_locked_work(locktype *lock) {
  int atomic_read(int const *p);
  void work();
  int myturn = fetch_and_add((&(lock->ticketnumber)));
  while ((atomic_read((&(lock->turn))) != myturn))
#pragma omp flush()
    work();
#pragma omp flush()
  fetch_and_add((&(lock->turn)));
}