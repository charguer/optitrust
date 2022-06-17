/**
 * @file arrayview.hpp
 * @author Bérenger BRAMAS (berenger.bramas@inria.fr)
 * @author Garip KUSOGLU (garip.kusoglu@inria.fr)
 * @author Stéphane GENAUD (genaud@unistra.fr)
 * @brief This file is to get a array view that is the same netherless the type
 * @version 1.0
 * @date 2021-10-18
 * 
 * @copyright MIT LICENSE 2021
 * 
 */
#ifndef ARRAYVIEW_HPP
#define ARRAYVIEW_HPP
/**
 * @brief This class is to get a array view that is the same netherless the DataType
 * 
 * @tparam DataType 
 */
template <class DataType>
class ArrayView{
    
    DataType* const data;

    long int dataSize;
public:

    ArrayView(DataType* inData, const long int inDataSize) 
        : data(inData), dataSize(inDataSize){}
    
};

#endif
