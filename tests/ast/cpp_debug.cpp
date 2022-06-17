// https://github.com/Miguel-Deniz/Vector-Implementation/tree/master/Vector%20Implementation

#include <cstddef>

template<class T> 
class Vector {
public:
	Vector();
	explicit Vector(int s);
private:
	size_t	_size;		// Number of elements in Vector
	T*		_elements;	// Pointer to first element of Vector
	size_t	_space;		// Total space used by Vector including
						// elements and free space.
};

template<class T>
inline Vector<T>::Vector(int s)
	:_size(s), _elements(new T[s], _space(s))
{
	for (int index = 0; index < _size; ++index)
		_elements[index] = T();
}

int main(){}