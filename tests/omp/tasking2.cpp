struct node {
  struct node *left;
  struct node *right;
};


extern void process (struct node*);


void postorder_traverse (struct node *p){
  if (p->left){
      postorder_traverse (p->left);
  }
  if (p->right){
    postorder_traverse (p->right);
  }
  process(p);
}
