/**
 * @file quicksort.cpp
 * @author Garip KUSOGLU (garip.kusoglu@inria.fr)
 * @author Bérenger BRAMAS (berenger.bramas@inria.fr)
 * @author Stéphane GENAUD (genaud@unistra.fr)
 * @brief This is a simple program to do a quicksort (with insetion sort at the end)
 * @version 1.0
 * @date 2021-10-18
 *
 * @copyright MIT LICENSE 2021
 *
 */
#include <random>

//#include "timer.hpp"

#include <iostream>
#include <stdexcept>
#include <unistd.h>
#include <cassert>
#include <chrono>
#include <thread>

#include "cpp_header.hpp"

/**
 * @brief Quicksort class to do a quicksort (with insetion sort at the end)
 *
 */
class Quicksort{

    /**
     * @brief Partitionning to find the pivot
     *
     * @param arr the array that is partitioned
     * @param leftLimit the left limit inside the array
     * @param rightLimit the right limit inside the array
     * @return int the pivot
     */
    static void Partition (int* outPivot, ArrayView<int> data)
    {
        int* arr = data.getData();
        const int pivot = arr[data.size()-1];
        int idxLeft = (0 - 1);
        for (int idxIter = 0; idxIter < data.size()-1 ; idxIter++){
            if (arr[idxIter] < pivot)
            {
                idxLeft += 1;
                std::swap(arr[idxLeft], arr[idxIter]);
            }
        }
        std::swap(arr[idxLeft + 1], arr[data.size()-1]);
        *outPivot = (idxLeft + 1);
    }

    /**
     * @brief Insertion sorting
     *
     * @param arr  the array that is sorted
     * @param leftLimit the left limit inside the array
     * @param rightLimit the right limit inside the array
     */
    static void InsertionSort(ArrayView<int> data)
    {
        int* arr = data.getData();
        for(int idx = 0 ; idx < data.size()-1 ; ++idx){
            int idxMin = idx;
            for(int idxIter = idxMin + 1 ; idxIter < data.size() ; ++idxIter){
                if(arr[idxMin] > arr[idxIter]){
                    idxMin = idxIter;
                }
            }
            int tmp = arr[idx];
            arr[idx] = arr[idxMin];
            arr[idxMin] = tmp;
        }
    }

    /**
     * @brief the core doubly recursif sorting function :
     *        1st recursion to sort lower half
     *        2nd recursion to sort upper half
     *
     * @param inOutData the array that is sorted
     * @param leftLimit the left limit inside the array
     * @param rightLimit the right limit inside the array
     */
    static void SortCore(ArrayView<int> data){
        int* inOutData = data.getData();
        if (1 >= data.size())
        {
            return;
        }
        if(data.size() <= 256 ){
            InsertionSort(data);
        }
        else{
            int pivot;

            Partition(&pivot, data);

            SortCore(data.getSubArray(0,pivot));
            SortCore(data.getSubArray(pivot + 1, data.size()));
        }
    }

public:

    /**
     * @brief public interface to call sortcore with feawer arguments
     *
     * @param inOutData the array that is sorted
     * @param inSize the array size
     */
    static int Sort(ArrayView<int> data){
        SortCore(data);
        return 1;
    }
};



/**
 * @brief Testing quicksort with big array
 *
 * @return 0
 */
int main(){
    const int Size = 10000000;
    std::vector<int> data(Size);

    std::mt19937 gen(0);
    // std::uniform_int_distribution<> dis(1, Size);

    // for(int idx = 0 ; idx < Size ; ++idx){
    //     data[idx] = dis(gen);
    // }
    int result = 0;
    //Timer timerSequential(result);
    result = Quicksort::Sort(ArrayView<int>(data.data(), int(data.size())));
    //timerSequential.stop(result);

    for(int idx = 1 ; idx < Size ; ++idx){
        if(data[idx-1] > data[idx]){
            std::cerr << "Array is not sorted!" << std::endl;
            exit(1);
        }
    }

    //std::cout << "Exec Time : " << timerSequential.getElapsed() << std::endl;

    return 0;
}
