#include <stdio.h>
#include <omp.h>

void socket_init (int socket_num){
  int n_procs;
  printf("Reporting in from socket num, thread num: %d %s\n", socket_num, omp_get_thread_num());

}


int main(){

  int n_sockets, socket_num;

  socket_init(socket_num);
}
