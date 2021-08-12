typedef struct node {
      int data;
      node * next;
} node;

void process(node * p)
{
    /* do work here */
}

void increment_list_items(node * head)
{
  node * p = head;
  while (p) {
    // p is firstprivate by default
    process(p);
    p = p->next;
  }
}

