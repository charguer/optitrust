typedef struct node {
  int data;
  node *next;
} node;

void process(node *p) {}

void increment_list_items(node *head) {
#pragma omp parallel
  {
#pragma omp single
    {
      node *p = head;
      while (p) {
#pragma omp task
        process(p);
        p = (p->next);
      }
    }
  }
}