int test_simpl(int x){
    
    return x;
}

// FIXME: should add early return
int test_one_branch(int x){
    if (x < 0){
      return -x;
    }
    return x;
}

int test_branches(int x){
  if (x > 0){
    return x;
  }
  else {
    return -x;
  }
}

int main(){
  return 0;
}