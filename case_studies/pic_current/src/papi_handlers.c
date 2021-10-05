#include <papi.h>   // constant  PAPI_OK
                    // functions PAPI_start_counters, PAPI_stop_counters, PAPI_strerror
                    // type      long_long
#include <stdlib.h> // type      size_t

/*****************************************************************************
 *                             Papi handlers                                 *
 *****************************************************************************/

void handle_error(int retval) {
    printf("PAPI error %d: %s\n", retval, PAPI_strerror(retval));
    exit(1);
}

char papi_event_names[108][90] = {
    "Level 1 data cache misses",
    "Level 1 instruction cache misses",
    "Level 2 data cache misses",
    "Level 2 instruction cache misses",
    "Level 3 data cache misses",
    "Level 3 instruction cache misses",
    "Level 1 cache misses",
    "Level 2 cache misses",
    "Level 3 cache misses",
    "Requests for a snoop",
    "Requests for exclusive access to shared cache line",
    "Requests for exclusive access to clean cache line",
    "Requests for cache line invalidation",
    "Requests for cache line intervention",
    "Level 3 load misses",
    "Level 3 store misses",
    "Cycles branch units are idle",
    "Cycles integer units are idle",
    "Cycles floating point units are idle",
    "Cycles load/store units are idle",
    "Data translation lookaside buffer misses",
    "Instruction translation lookaside buffer misses",
    "Total translation lookaside buffer misses",
    "Level 1 load misses",
    "Level 1 store misses",
    "Level 2 load misses",
    "Level 2 store misses",
    "Branch target address cache misses",
    "Data prefetch cache misses",
    "Level 3 data cache hits",
    "Translation lookaside buffer shootdowns",
    "Failed store conditional instructions",
    "Successful store conditional instructions",
    "Total store conditional instructions",
    "Cycles Stalled Waiting for memory accesses",
    "Cycles Stalled Waiting for memory Reads",
    "Cycles Stalled Waiting for memory writes",
    "Cycles with no instruction issue",
    "Cycles with maximum instruction issue",
    "Cycles with no instructions completed",
    "Cycles with maximum instructions completed",
    "Hardware interrupts",
    "Unconditional branch instructions",
    "Conditional branch instructions",
    "Conditional branch instructions taken",
    "Conditional branch instructions not taken",
    "Conditional branch instructions mispredicted",
    "Conditional branch instructions correctly predicted",
    "FMA instructions completed",
    "Instructions issued",
    "Instructions completed",
    "Integer instructions",
    "Floating point instructions",
    "Load instructions",
    "Store instructions",
    "Branch instructions",
    "Vector/SIMD instructions (could include integer)",
    "Cycles stalled on any resource",
    "Cycles the FP unit(s) are stalled",
    "Total cycles",
    "Load/store instructions completed",
    "Synchronization instructions completed",
    "Level 1 data cache hits",
    "Level 2 data cache hits",
    "Level 1 data cache accesses",
    "Level 2 data cache accesses",
    "Level 3 data cache accesses",
    "Level 1 data cache reads",
    "Level 2 data cache reads",
    "Level 3 data cache reads",
    "Level 1 data cache writes",
    "Level 2 data cache writes",
    "Level 3 data cache writes",
    "Level 1 instruction cache hits",
    "Level 2 instruction cache hits",
    "Level 3 instruction cache hits",
    "Level 1 instruction cache accesses",
    "Level 2 instruction cache accesses",
    "Level 3 instruction cache accesses",
    "Level 1 instruction cache reads",
    "Level 2 instruction cache reads",
    "Level 3 instruction cache reads",
    "Level 1 instruction cache writes",
    "Level 2 instruction cache writes",
    "Level 3 instruction cache writes",
    "Level 1 total cache hits",
    "Level 2 total cache hits",
    "Level 3 total cache hits",
    "Level 1 total cache accesses",
    "Level 2 total cache accesses",
    "Level 3 total cache accesses",
    "Level 1 total cache reads",
    "Level 2 total cache reads",
    "Level 3 total cache reads",
    "Level 1 total cache writes",
    "Level 2 total cache writes",
    "Level 3 total cache writes",
    "Floating point multiply instructions",
    "Floating point add instructions",
    "Floating point divide instructions",
    "Floating point square root instructions",
    "Floating point inverse instructions",
    "Floating point operations",
    "Floating point operations; optimized to count scaled single precision vector operations",
    "Floating point operations; optimized to count scaled double precision vector operations",
    "Single precision vector/SIMD instructions",
    "Double precision vector/SIMD instructions",
    "Reference clock cycles"
};

void start_diag_papi(FILE** diag_file, char* diag_name, int num_events, int Events[num_events]) {
    *diag_file = fopen(diag_name, "w");
    fprintf(*diag_file, "Iteration");
    for (size_t i = 0; i < num_events; i++)
        fprintf(*diag_file, " | %s", papi_event_names[Events[i]-0x80000000]);
    fprintf(*diag_file, "\n");
    /* Start counting events */
    if (PAPI_start_counters(Events, num_events) != PAPI_OK)
        handle_error(1);
}

void stop_diag_papi(FILE* diag_file, int num_events, long_long values[num_events]) {
    /* Stop counting events */
    if (PAPI_stop_counters(values, num_events) != PAPI_OK)
        handle_error(1);
    fclose(diag_file);
}

