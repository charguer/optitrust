#include <vector>

void iterator_example (){
  std::vector<int> vec(23);
  std::vector<int>::iterator it;

  for(it = vec.begin(); it < vec.end(); it++){
    // do work with *it //
  }
  
}
