
const int LARGE_NUMBER =  10000000;
double item[LARGE_NUMBER];
void process(double);

int main()
{
  int i;
  for (i=0; i<LARGE_NUMBER; i++){
    process(item[i]);
  }
}
