#include <omp.h>

void start_background_work(void);
void check_step(void);
void print_results(void);

struct tree_node{
  struct tree_node *left;
  struct tree_node *right;
};

typedef struct tree_node* tree_type;
extern void init_tree(tree_type);
const int max_steps = 100;

void compute_something (tree_type tree){
  // some computation
}
void compute_tree (tree_type tree){
  if(tree->left){
    compute_tree(tree->left);
  }
  if(tree->right){
    compute_tree(tree->right);
  }
  compute_something(tree);

}

int main(){
  int i;
  tree_type tree;
  init_tree (tree);

  start_background_work();
  for(i = 0; i < max_steps; i++){
    compute_tree(tree);
    check_step();
  }
  print_results();
  return 0;
}
