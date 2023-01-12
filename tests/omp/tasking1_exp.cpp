struct node {
  struct node* left;
  struct node* right;
};

void process(struct node*);

void traverse(struct node* p) {
  if (p->left) {
#pragma omp task
    traverse(p->left);
  }
  if (p->right) {
#pragma omp task
    traverse(p->right);
  }
  process(p);
}
