template <class DataType>
class ArrayView{
    DataType* const data;

    long int dataSize;
public:

    ArrayView(DataType* inData, const long int inDataSize) 
        : data(inData), dataSize(inDataSize){}
    
};
