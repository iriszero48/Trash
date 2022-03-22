#pragma once

#include <vector>
#include <algorithm>
#include <iterator>
#include <functional>
#include <numeric>

namespace Seq
{
	template<typename T>
	class List
	{
	public:
		List(const std::vector<T>& cont)
		{
			std::copy(cont.begin(), cont.end(), std::back_inserter(data));
		}

        List(const std::string_view& cont)
		{
			std::copy(cont.begin(), cont.end(), std::back_inserter(data));
		}

		template<typename Array, uint64_t Size>
		List(Array (&arr)[Size])
		{
			std::copy_n(arr, Size, std::back_inserter(data));
		}

		template<typename It>
		List(It begin, It end)
		{
			std::copy(begin, end, std::back_inserter(data));
		}

		template<uint64_t Size>
		std::array<T, Size> ToArray() const
		{
			std::array<T, Size> buf{};
			std::copy_n(data.begin(), Size, buf.begin());
			return buf;
		}

		template<typename Func>
		List<T> Filter(Func func) const
		{
			std::vector<T> buf;
			std::copy_if(data.begin(), data.end(), std::back_inserter(buf), func);
			return buf;
		}

		template<typename Func>
		decltype(auto) Map(Func func) const
		{
			using Type = decltype(func(data.at(0)));
			std::vector<Type> buf;
			std::transform(data.begin(), data.end(), std::back_inserter(buf), func);
			return List<Type>(buf);
		}

        template<typename Func>
		decltype(auto) Mapi(Func func) const
		{
			using Type = decltype(func(0, data.at(0)));
			std::vector<Type> buf;
			std::transform(data.begin(), data.end(), std::back_inserter(buf), [&, i = 0](const auto& x) mutable { return func(i++, x); });
			return List<Type>(buf);
		}

		template<typename Func>
		decltype(auto) Choose(Func func) const
		{
			return Map(func).Filter([](const auto& x) { return x.has_value(); }).Map([](const auto& x) { return x.value(); });
		}

		decltype(auto) Concat(T token) const
		{
			return std::accumulate(data.begin() + 1, data.end(), *data.begin(), [&](auto& s, auto& x) { return s + token + x; });
		}

        List<List<T>> ChunkBySize(uint64_t size) const
        {
            std::vector<List<T>> buf;
            const auto n = data.size() / size;

            for (uint64_t i = 0; i < n; i++)
            {
                std::vector<T> elem{};
                std::copy_n(data.begin() + i * size, size, std::back_inserter(elem));
                buf.emplace_back(elem);
            }

            if (data.size() % size != 0)
            {
                std::vector<T> elem{};
                std::copy(data.begin() + n * size, data.end(), std::back_inserter(elem));
                buf.emplace_back(elem);
            }

            return buf;
        }

        List<T> rev() const
        {
            std::vector<T> buf;
            std::copy(data.rbegin(), data.rend(), std::back_inserter(buf));
            return buf;
        }

        T Head() const
        {
            return data.at(0);
        }

        decltype(auto) Length() const
        {
            return data.size();
        }

        std::vector<T> Get() const
        {
            return data;
        }

        List<T> Append(const List<T>& lst) const
        {
            std::vector<T> buf = data;
            const auto val = lst.Get();
            std::copy(val.begin(), val.end(), std::back_inserter(buf));
            return buf;
        }

        template<typename Func>
        auto Reduce(Func func) const
        {
            if (data.size() == 1) return data.at(0);
			return std::accumulate(data.begin() + 1, data.end(), *data.begin(), func);
        }

        T Item(const uint64_t index) const
        {
            return data.at(index);
        }

        template<typename Func>
        List<T> Fork(Func func) const
        {
            func(*this);
            return *this;
        }

        uint64_t IndexOf(const T& val) const
        {
            return std::find(data.begin(), data.end(), val) - data.begin();
        }

        decltype(auto) Sum() const
        {
            return std::accumulate(data.begin() + 1, data.end(), *data.begin());
        }

        List<T> Take(const uint64_t size) const
        {
            std::vector<T> buf{};
            std::copy_n(data.begin(), size, std::back_inserter(buf));
            return buf;
        }

        decltype(auto) begin() const
        {
            return data.begin();
        }

        decltype(auto) end() const
        {
            return data.end();
        }

        static List<T> Create(const uint64_t size, const T& val)
        {
            std::vector<T> buf(size, val);
            return buf;
        }

        template<typename Func>
        static List<T> Init(const uint64_t size, Func func)
        {
			std::vector<T> buf(size);
            std::generate_n(buf.begin(), size, [&, i = 0]() mutable { return func(i++); });
			return buf;
        }
	private:
		std::vector<T> data{};
	};
}
