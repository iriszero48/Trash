#pragma once

#include <fstream>

namespace CSV
{
	class CsvFile
	{
		std::ofstream fs;
		bool isFirst = true;
		const std::string separator = ",";
		const std::string escapeSeq = "\"";
		const std::string specialChars = "\"";
	public:
		CsvFile(const std::string& filename);
		
		CsvFile(const CsvFile& file) = delete;

		CsvFile(CsvFile&& file);

		CsvFile& operator=(const CsvFile& file) = delete;

		CsvFile& operator=(CsvFile&& file);

		CsvFile() = default;

		~CsvFile();

		void Flush();

		void EndRow();

		CsvFile& operator << (CsvFile& (*val)(CsvFile&));

		CsvFile& operator << (const char* val);

		CsvFile& operator << (const std::string& val);

		template<typename T>
		CsvFile& operator << (const T& val)
		{
			return Write(val);
		}

		static CsvFile& endl(CsvFile& file);

		static CsvFile& Flush(CsvFile& file);

	private:
		template<typename T>
		CsvFile& Write(const T& val)
		{
			if (!isFirst)
			{
				fs << separator;
			}
			else
			{
				isFirst = false;
			}
			fs << val;
			return *this;
		}

		[[nodiscard]] std::string Escape(const std::string& val) const;
	};
}
