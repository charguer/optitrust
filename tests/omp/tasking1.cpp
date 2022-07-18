struct node {
  struct node *left;
  struct node *right;
};


extern void process (struct node*);

void traverse (struct node *p){
  if (p->left){
    traverse (p->left);
  }
  if (p->right){
    traverse (p->right);
  }
  process(p);
}
