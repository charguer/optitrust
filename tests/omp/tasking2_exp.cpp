struct node {
  struct node* left;
  struct node* right;
};

void process(struct node*);

void postorder_traverse(struct node* p) {
  if (p->left) {
#pragma omp task
    postorder_traverse(p->left);
  }
  if (p->right) {
#pragma omp task
    postorder_traverse(p->right);
  }
#pragma omp taskwait
  process(p);
}
