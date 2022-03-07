#ifndef PIC_VERT_PAPI_HANDLERS
#define PIC_VERT_PAPI_HANDLERS

#include <papi.h> // type long_long

/*****************************************************************************
 *                             Papi handlers                                 *
 *****************************************************************************/

void handle_error(int retval);

void start_diag_papi(FILE** diag_file, char* diag_name, int num_events, int Events[num_events]);

void stop_diag_papi(FILE* diag_file, int num_events, long_long values[num_events]);

#endif // ifndef PIC_VERT_PAPI_HANDLERS
