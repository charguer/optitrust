#include <random>
#include <iostream>
#include <stdexcept>
#include <unistd.h>

class Quicksort{
    static void Partition (int* outPivot, int arr[], const int rightLimit)
    {
        const int pivot = arr[rightLimit];
        int idxLeft = (0 - 1);
        for (int idxIter = 0; idxIter <= rightLimit - 1; idxIter++){
            if (arr[idxIter] < pivot)
            {
                idxLeft += 1;
                std::swap(arr[idxLeft], arr[idxIter]);
            }
        }
        std::swap(arr[idxLeft + 1], arr[rightLimit]);
        *outPivot = (idxLeft + 1);
    }



    static void InsertionSort(int arr[], const int rightLimit)
    {
        for(int idx = 0 ; idx < rightLimit; ++idx){
            int idxMin = idx;
            for(int idxIter = idxMin + 1 ; idxIter <= rightLimit ; ++idxIter){
                if(arr[idxMin] > arr[idxIter]){
                    idxMin = idxIter;
                }
            }
            int tmp = arr[idx];
            arr[idx] = arr[idxMin];
            arr[idxMin] = tmp;
        }
    }

    static void SortCore(int inOutData[], const int rightLimit){
        if (0 >= rightLimit)
        {
            return;
        }
        if(rightLimit<=256){
            InsertionSort(inOutData, rightLimit);
        }
        else{
            int pivot;

            Partition(&pivot, inOutData, rightLimit);

            SortCore(&inOutData[0], pivot - 1);
            SortCore(&inOutData[pivot + 1], rightLimit -(pivot + 1));
        }
    }

public:
    static void Sort(int inOutData[], const int inSize){
        SortCore(inOutData, inSize);
    }
};



int main(){
    const int Size = 100000000;
    std::vector<int> data(Size);

    std::mt19937 gen(0);
    std::uniform_int_distribution<> dis(1, Size);

    for(int idx = 0 ; idx < Size ; ++idx){
        data[idx] = dis(gen);
    }

    Quicksort::Sort(data.data(), int(data.size()));

    for(int idx = 1 ; idx < Size ; ++idx){
        if(data[idx-1] > data[idx]){
            throw std::runtime_error("Array is not sorted!");
        }
    }

    // std::cout << "Done" << std::endl;

    return 0;
}
