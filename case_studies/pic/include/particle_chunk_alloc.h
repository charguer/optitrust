//==========================================================================
// Manual memory management for chunks

// You need one free list per processor
unsigned int FREELIST_SIZE;
// free_index[thread_id][0] will store the number of free chunks available
// for thread number thread_id. Other subscripts are not used, but this is a
// matrix and not an array to avoid false sharing (each row is aligned
// on the cache line size, usually 64 bytes on modern Intel architectures).
#define FREE_INDEX(i) free_index[i][0]
int** free_index;
#ifdef PIC_VERT_TESTING
// Could also be optimised for false sharing, but it's only for testing so...
int* nb_malloc;
int* nb_free;
#endif

chunk*** free_chunks;    // Freelists
chunk** all_free_chunks; // Stores the free chunks during the initialization phase, before they are put into freelists
int number_of_spare_chunks_per_parity; // Number of chunks needed to initialize each bag in the append phase.
int** spare_chunks_ids;                // Bijection between [0 ; nb_bags_per_cell[ x [0 ; num_cells_3d[ and [0 ; number_of_spare_chunks_per_parity[
int num_threads;

// Cumulative sum of the freelist sizes.
// It is useless to allocate this array with only one cell per cache line,
// as all the threads will access all the cells, and not, like other arrays
// of this kind, only thread index thread_id will access array[thread_id].
int* cumulative_free_indexes;
int bag_last_spare_chunk_to_be_used;
#ifdef SPARE_LOC_OPTIMIZED
enum SPARE_CHUNK_LOCATION {
    SPARE_CHUNK_LOCATION_ID_THREAD,
    SPARE_CHUNK_LOCATION_OFFSET,
    SPARE_CHUNK_LOCATION_MAX_OFFSET,
    SPARE_CHUNK_LOCATION_NB_INFO // Always has to be last if you update this enum !
};
int** spare_chunk_location;
#endif

/*
 * Take a chunk from the freelist of thread_id. If the freelist is empty,
 * allocate a new one. This function should only be called during the particle
 * loop. During the append phase, the needed chunks should be found
 * in all the freelists, depending of the cumulative freelist sizes.
 *
 * @param[in] thread_id the index of the thread asking for a new chunk.
 * @return    a chunk almost ready to be filled with particles (set size to 0 before).
 */
chunk* manual_chunk_alloc(int thread_id) {
  if (FREE_INDEX(thread_id) > 0) {
    return free_chunks[thread_id][--FREE_INDEX(thread_id)];
  } else {
#ifdef PIC_VERT_TESTING
    nb_malloc[thread_id]++;
#endif
    return (chunk*) malloc(sizeof(chunk));
  }
}

/*
 * Put a chunk back into the freelist of thread_id. This function is only
 * called during the particle loop, on a chunk that has just been iterated on.
 * If that list is full, free the chunk.
 *
 * @param[in] c         the chunk to be put back in the freelist.
 * @param[in] thread_id the index of the thread asking for this release.
 */
void manual_chunk_free(chunk* c, int thread_id) {
  if (FREE_INDEX(thread_id) < FREELIST_SIZE) {
    free_chunks[thread_id][FREE_INDEX(thread_id)++] = c;
  } else {
#ifdef PIC_VERT_TESTING
    nb_free[thread_id]++;
#endif
    free(c);
  }
}

#ifdef SPARE_LOC_OPTIMIZED
/*
 * Locate where in the freelists is the chunk that should be put on the
 * cell id_cell (in [0 ; ncx * ncy * ncz[), in the bag id_bag (in [0 ; 9[)
 * for the next iteration, by the thread thread_id.
 *
 * The location is stored into spare_chunk_location[thread_id].
 * The first number (in SPARE_CHUNK_LOCATION_ID_THREAD) tells in the freelist
 * of which thread this chunk should be found.
 * The second number (in SPARE_CHUNK_LOCATION_OFFSET) tells at which position
 * in that freelist this chunk should be found.
 * The last number (SPARE_CHUNK_LOCATION_MAX_OFFSET) tells at which index in
 * this freelist you should re-call this function, because you are reaching
 * the end of the freelist. As long as you do not reach this number, you can
 * locate the next chunk in the same freelist, by just increasing the second
 * number (in SPARE_CHUNK_LOCATION_OFFSET).
 *
 * @param[in] id_bag    the index of the bag (in [0 ; 9[) in which to put a chunk.
 * @param[in] id_cell   the index of the cell (in [0 ; ncx * ncy * ncz[) in which
 *                      to put a chunk.
 * @param[in] thread_id the index of the thread that asks for a chunk.
 */
void locate_spare_chunk(int id_bag, int id_cell, int thread_id) {
  int id_chunk = spare_chunks_ids[id_bag][id_cell];
  int k;
  for (k = num_threads - 1; k >= 0; k--)
    if (cumulative_free_indexes[k] <= id_chunk)
      break;
  int index = id_chunk - cumulative_free_indexes[k];
  if (index < 0 || index >= FREE_INDEX(k)) {
    printf("Not enough free chunks in thread %d !\n", k);
    printf("Maybe did you forgot to call compute_cumulative_free_list_sizes and/or update_free_list_sizes ?\n");
    exit(EXIT_FAILURE);
  }
  spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_ID_THREAD ] = k;
  spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_OFFSET    ] = (k == bag_last_spare_chunk_to_be_used)
    ? index + FREE_INDEX(k) - (number_of_spare_chunks_per_parity - cumulative_free_indexes[k])
    : index;
  spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_MAX_OFFSET] = FREE_INDEX(k);
}
#endif

// In the initial phase, instantiate the argument thread_id with the value THREAD_INITIAL
// to assume that all free chunks are stored in the free list of processor THREAD_ZERO.
const int THREAD_INITIAL = -1;
const int THREAD_ZERO = 0;

chunk* manual_obtain_chunk_initial() {
  // We are never too careful (-:
  if (FREE_INDEX(THREAD_ZERO) < 1) {
    fprintf(stderr, "Not enough chunks in all_free_chunks. Check its allocation.\n");
    exit(EXIT_FAILURE);
  }
  return all_free_chunks[--FREE_INDEX(THREAD_ZERO)];
}

/*
 * Obtain a chunk from the free list; used by bag_init (i.e. outside of the particle loop)
 * ARTHUR: document this function
 */
chunk* manual_obtain_chunk(int id_bag, int id_cell, int thread_id) {
  if (thread_id == THREAD_INITIAL) {
    return manual_obtain_chunk_initial();
  }
#ifdef SPARE_LOC_OPTIMIZED
  if (spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_OFFSET] >= spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_MAX_OFFSET])
    locate_spare_chunk(id_bag, id_cell, thread_id);
  chunk* c = free_chunks[spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_ID_THREAD]]
                        [spare_chunk_location[thread_id][SPARE_CHUNK_LOCATION_OFFSET]++];
#else
  int id_chunk = spare_chunks_ids[id_bag][id_cell];
  int k;
  for (k = num_threads - 1; k >= 0; k--)
    if (cumulative_free_indexes[k] <= id_chunk)
      break;
  int offset = id_chunk - cumulative_free_indexes[k];
  if (offset < 0 || offset >= FREE_INDEX(k)) {
    printf("Not enough free chunks in thread %d !\n", k);
    printf("Maybe did you forgot to call compute_cumulative_free_list_sizes and/or update_free_list_sizes ?\n");
    exit(EXIT_FAILURE);
  }
  chunk* c = free_chunks[k][FREE_INDEX(k) - 1 - offset];
#endif
  return c;
}

/*
 * Compute the cumulative sum of the freelist sizes. This cumulative sum will
 * be used to locate needed chunks in the append phase. It should therefore
 * be called before the append phase.
 *
 * WARNING : This function is not thread-safe, and should be called only by
 * the master thread.
 *
 * Reminder : cumulative_free_indexes[0] = 0
 *            cumulative_free_indexes[i] = sum(j = 0; j < i) FREE_INDEX(j).
 */
void compute_cumulative_free_list_sizes() {
  int k;
  cumulative_free_indexes[0] = 0;
  for (k = 1; k < num_threads; k++)
    cumulative_free_indexes[k] = cumulative_free_indexes[k - 1] + FREE_INDEX(k - 1);
  int nb_free_chunks = cumulative_free_indexes[num_threads - 1] + FREE_INDEX(num_threads - 1);
  // We are never too careful :)
  if (nb_free_chunks < number_of_spare_chunks_per_parity) {
    int nb_chunks_to_allocate = number_of_spare_chunks_per_parity - nb_free_chunks;
    printf("Not enough free chunks in the free lists ! We must malloc %d chunks.\n", nb_chunks_to_allocate);
    // Allocates new free chunks, all in the freelist of thread 0.
    // Remember that FREELIST_SIZE = 2 * number_of_spare_chunks_per_parity, so
    // there is enough indexes in that list to put all the new chunks here.
    int nb_allocated_chunks = 0;
    while (nb_allocated_chunks < nb_chunks_to_allocate) {
      free_chunks[THREAD_ZERO][FREE_INDEX(THREAD_ZERO)++] = (chunk*)malloc(sizeof(chunk));
      nb_allocated_chunks++;
#ifdef PIC_VERT_TESTING
      nb_malloc[THREAD_ZERO]++;
#endif
    }
    compute_cumulative_free_list_sizes();
  }
#ifdef SPARE_LOC_OPTIMIZED
  for (k = 0; k < num_threads; k++) {
    spare_chunk_location[k][SPARE_CHUNK_LOCATION_ID_THREAD ] = 0;
    spare_chunk_location[k][SPARE_CHUNK_LOCATION_OFFSET    ] = 0;
    spare_chunk_location[k][SPARE_CHUNK_LOCATION_MAX_OFFSET] = 0;
  }
#endif
  for (k = num_threads - 1; k >= 0; k--)
    if (cumulative_free_indexes[k] <= number_of_spare_chunks_per_parity) {
      bag_last_spare_chunk_to_be_used = k;
      return;
  }
}

/*
 * Update the freelist sizes after the append phase, according to the number
 * of chunks that were taken in each freelist. It should therefore
 * be called after the append phase.
 *
 * WARNING : This function is not thread-safe, and should be called only by
 * the master thread.
 */
void update_free_list_sizes() {
  for (int i = 0; i < bag_last_spare_chunk_to_be_used; i++)
    FREE_INDEX(i) = 0;
  FREE_INDEX(bag_last_spare_chunk_to_be_used) -= number_of_spare_chunks_per_parity - cumulative_free_indexes[bag_last_spare_chunk_to_be_used];
}



// ARTHUR: document better why we have alloc_chunk and obtain_chunk
// ARTHUR: simplify init_all_chunks and  init_freelists
// ARTHUR: document what is number_of_spare_chunks_per_parity
// ARTHUR: cumulative_nb_chunks_per_cell -> would be nicer to have one extra cell
