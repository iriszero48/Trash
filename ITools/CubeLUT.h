#pragma once

#include <string> 
#include <vector> 
#include <fstream>
#include <variant>
#include <sstream>
#include <filesystem>


namespace Lut
{
	template <typename T = uint8_t>
	struct ColorRgb
	{
		T R;
		T G;
		T B;

		ColorRgb() : R(0), G(0), B(0) {}

		explicit ColorRgb(const T v) : R(v), G(v), B(v) {}

		ColorRgb(const T r, const T g, const T b) : R(r), G(g), B(b) {}

		template <typename Arr>
		explicit ColorRgb(const Arr (&arr) [3]) : R(arr[0]), G(arr[1]), B(arr[2]) {}
	};

	template <typename Impl>
	struct ITable
	{
		template <typename ...Args>
		decltype(auto) At(Args&&...args)
		{
			return static_cast<Impl*>(this)->At(std::forward<Args>(args)...);
		}

		template <typename ...Args>
		decltype(auto) At(Args&&...args) const
		{
			return static_cast<Impl*>(this)->At(std::forward<Args>(args)...);
		}

		size_t Length()
		{
			return static_cast<Impl*>(this)->Length();
		}

		template <typename ...Args>
		decltype(auto) SafeAt(Args&&...args)
		{
			return static_cast<Impl*>(this)->SafeAt(std::forward<Args>(args)...);
		}

		template <typename ...Args>
		decltype(auto) SafeAt(Args&&...args) const
		{
			return static_cast<Impl*>(this)->SafeAt(std::forward<Args>(args)...);
		}

		decltype(auto) GetRawData() const
		{
			return static_cast<Impl*>(this)->GetRawData();
		}
	};

	struct Table1D : ITable<Table1D>
	{
		using ElemType = ColorRgb<float>;

		Table1D() = default;

		explicit Table1D(const size_t elemSize) : data(elemSize) {}

		[[nodiscard]] size_t Length() const { return data.size(); }

		ElemType& At(const size_t i)
		{
			return data[i];
		}

		[[nodiscard]] const ElemType& At(const size_t i) const
		{
			return data.at(i);
		}

		[[nodiscard]] const std::vector<ElemType>& GetRawData() const
		{
			return data;
		}

	private:
		std::vector<ElemType> data{};
	};

	struct Table3D : ITable<Table3D>
	{
		using ElemType = ColorRgb<float>;

		explicit Table3D(const size_t elemSize): data(elemSize * elemSize * elemSize), elemSize(elemSize) {}

		[[nodiscard]] size_t Length() const { return elemSize; }

		ElemType& At(const size_t r, const size_t g, const size_t b)
		{
			return data[Pos(r, g, b)];
		}

		[[nodiscard]] const ElemType& At(const size_t r, const size_t g, const size_t b) const
		{
			return data[Pos(r, g, b)];
		}

		[[nodiscard]] const std::vector<ElemType>& GetRawData() const
		{
			return data;
		}
		
	private:
		std::vector<ElemType> data{};
		size_t elemSize{};

		[[nodiscard]] size_t Pos(const size_t r, const size_t g, const size_t b) const
		{
			return r * elemSize * elemSize + g * elemSize + b;
		}
	};

	class CubeLut
	{
	public:
		using Row = ColorRgb<float>;
		using TableType = std::variant<Table1D, Table3D>;

		enum class Dim { _1D, _3D };

		enum class LutState
		{
			OK = 0, NotInitialized = 1,
			ReadError = 10, WriteError, PrematureEndOfFile, LineError,
			UnknownOrRepeatedKeyword = 20, TitleMissingQuote, DomainBoundsReversed,
			LUTSizeOutOfRange, CouldNotParseTableData
		};

		std::string Title;
		ColorRgb<float> DomainMin;
		ColorRgb<float> DomainMax;

		CubeLut() : status(LutState::NotInitialized) {}

		[[nodiscard]] const TableType& GetTable() const;
		[[nodiscard]] Dim GetDim() const;
		[[nodiscard]] uint64_t Length() const;

		LutState LoadCubeFile(std::ifstream& infile);
		LutState SaveCubeFile(std::ofstream& outfile);

		static CubeLut FromCubeFile(const std::filesystem::path& file);

	private:
		LutState status;
		TableType table{};

		std::string ReadLine(std::ifstream& infile, char lineSeparator);
		Row ParseTableRow(const std::string& lineOfText);
	};

}
