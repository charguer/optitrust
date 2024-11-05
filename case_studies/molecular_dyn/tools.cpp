#include "tools.hpp"

std::mt19937 generator;
std::uniform_real_distribution<> distribution;

void initialize_random_number_generator(double box_width) {
  generator = std::mt19937(0);
  distribution = std::uniform_real_distribution<>(0.0, box_width);
}

double random_number() {
  return distribution(generator);
}
