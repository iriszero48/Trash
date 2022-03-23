#pragma once

#include <cstdint>

namespace Enumerable
{
	template<typename T>
	class Range
	{
		T begVal = 0;
		T endVal;
	
	public:
		class iterator
		{
			T val;
		
		public:
			using iterator_category = std::random_access_iterator_tag;
			using value_type = T;
			using difference_type = std::int64_t;
			using pointer = const T*;
			using reference = const T&;

			iterator() : val(0) {}
			iterator(T val) : val(val) {}
			
			iterator& operator++() { ++val; return *this; }
			iterator operator++(int) { iterator retVal = *this; ++(*this); return retVal; }
			
			bool operator==(iterator other) const { return val == other.val; }
			bool operator!=(iterator other) const { return !(*this == other); }
			bool operator<(iterator other) const { return val < other.val; }
			
			reference operator*() const { return val; }
			value_type operator+(iterator other) const { return val + other.val; }
			difference_type operator-(iterator other) const { return val - other.val; }
		};

		Range(T count) :endVal(count) {}
		Range(T start, T count) : begVal(start), endVal(start + count) {}
		
		iterator begin() { return iterator(begVal); }
		iterator end() { return iterator(endVal); }
	};
}
