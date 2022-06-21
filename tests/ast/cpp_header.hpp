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
    /**
     * @brief The actual data
     * 
     */
    DataType* const data;

    /**
     * @brief The data size in memory blocks (cases)
     * 
     */
    long int dataSize;
public:

    /**
     * @brief Construct a new Array View object
     * 
     * @param inData The actual data
     * @param inDataSize The data size in memory blocks (cases)
     */
    // ArrayView(DataType* inData, const long int inDataSize) 
    //     : data(inData), dataSize(inDataSize){}
    // encoded as
    ArrayView(DataType* inData, const long int inDataSize)  {
      this->data = inData;  /*  @annot_member_initializer */
      this->dataSize = inDataSize; /* @annot_member_initializer */
    }

    /**
     * @brief Construct a new Array View object
     * 
     */
    ArrayView (){
      this->data = NULL;
      this->dataSize = 0;
    }
    
    ArrayView(const ArrayView&) = default;

    /**
     * @brief Construct a new Array View object
     * 
     * @return ArrayView& 
     */
    ArrayView& operator=(const ArrayView&) = default;

    /**
     * @brief Construct a new Array View object
     * 
     */
    ArrayView(ArrayView&&) = default;

    /**
     * @brief Construct a new Array View object
     * 
     * @return ArrayView& 
     */
    ArrayView& operator=(ArrayView&&) = default;

    /**
     * @brief Get the Data object
     * 
     * @return DataType* 
     */
    DataType* getData(){
        return  data;
    }

    /**
     * @brief Get the Data object
     * 
     * @return const DataType* 
     */
    const DataType* getData() const {
        return  data;
    }

    /**
     * @brief Get the Data size
     * 
     * @return long int 
     */
    long int size() const{
        return dataSize;
    }

    /**
     * @brief access data at idx position
     * 
     * @param idx position to access
     * @return DataType& 
     */
    DataType& operator[](long int idx){
        return data[idx];
    }

    /**
     * @brief access data at idx position
     * 
     * @param idx position to access
     * @return const DataType& 
     */
    const DataType& operator[](long int idx) const {
        return data[idx];
    }

    /**
     * @brief Get the a sub Array of The curent ArrayView
     * 
     * @param inStartingIdx starting position
     * @param inEndingIdx ending position
     * @return ArrayView<DataType> 
     */
    auto getSubArray(const long int inStartingIdx, const long int inEndingIdx){
        return ArrayView<DataType>(data+inStartingIdx, inEndingIdx-inStartingIdx);
    }

    /**
     * @brief Get the a sub Array of The curent ArrayView
     * 
     * @param inStartingIdx starting position
     * @param inEndingIdx ending position
     * @return ArrayView<const DataType> 
     */
    auto getSubArray(const long int inStartingIdx, const long int inEndingIdx) const{
        return ArrayView<const DataType>(data+inStartingIdx, inEndingIdx-inStartingIdx);
    }
};

#endif
