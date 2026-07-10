void magic_barrier();
void work();

void kernel_sequence() {
  magic_barrier();
  work();
  magic_barrier();
}
