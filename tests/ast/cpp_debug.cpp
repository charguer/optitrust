template <class DataType>
class ArrayView{
    
    DataType* const data;

    
    long int dataSize;
public:

    
    // ArrayView(DataType* inData, const long int inDataSize) 
    //     : data(inData), dataSize(inDataSize){}
    ArrayView(DataType* inData, const long int inDataSize)  {
      data = inData;
      dataSize = inDataSize;
    }
        

    
    ArrayView(const ArrayView&) = default;

    
    ArrayView& operator=(const ArrayView&) = default;

    
    ArrayView(ArrayView&&) = default;

    
    ArrayView& operator=(ArrayView&&) = default;

    
    DataType* getData(){
        return  data;
    }
 
    
    const DataType* getData() const {
        return  data;
    }

    
    long int size() const{
        return dataSize;
    }

    
    DataType& operator[](long int idx){
        return data[idx];
    }

    
    const DataType& operator[](long int idx) const {
        return data[idx];
    }

    
    auto getSubArray(const long int inStartingIdx, const long int inEndingIdx){
        return ArrayView<DataType>(data+inStartingIdx, inEndingIdx-inStartingIdx);
    }

    
    auto getSubArray(const long int inStartingIdx, const long int inEndingIdx) const{
        return ArrayView<const DataType>(data+inStartingIdx, inEndingIdx-inStartingIdx);
    }
};

int main(){}