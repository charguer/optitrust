int* t;


int main(){
    int N = 10;
    
    for(int i = 0; i < N; i++)
    {
        t[i] = 0;
    }
    for(int j = 0; j < N; j += 2)
    {
        t[j] = 0;
    }
    
    return 0;
    
}