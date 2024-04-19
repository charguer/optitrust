#include <random>

extern std::mt19937 generator;
extern std::uniform_real_distribution<> distribution;

void initialize_random_number_generator(double box_width);
double random_number();