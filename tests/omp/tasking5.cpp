#define LARGE_NUMBER 10000000
double item[LARGE_NUMBER];
extern void process(double);

int main()
{
  for (int i=0; i<LARGE_NUMBER; i++)
    process(item[i]);
}
