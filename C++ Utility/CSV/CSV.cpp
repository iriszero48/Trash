#include <sstream>
#include <utility>

#include "CSV.h"

namespace CSV
{
	CsvFile::CsvFile(const std::string& filename)
	{
		fs.exceptions(std::ios::failbit | std::ios::badbit);
		fs.open(filename);
	}
	
	CsvFile::CsvFile(CsvFile&& file)
	{
		fs = std::move(file.fs);
	}

	CsvFile& CsvFile::operator=(CsvFile&& file)
	{
		fs = std::move(file.fs);
		return *this;
	}
	
	CsvFile::~CsvFile()
	{
		Flush();
		fs.close();
	}

	void CsvFile::Flush()
	{
		fs.flush();
	}

	void CsvFile::EndRow()
	{
		fs << "\n";
		isFirst = true;
	}

	CsvFile& CsvFile::operator<<(CsvFile& (*val)(CsvFile&))
	{
		return val(*this);
	}

	CsvFile& CsvFile::operator<<(const char* val)
	{
		return Write(Escape(val));
	}

	CsvFile& CsvFile::operator<<(const std::string& val)
	{
		return Write(Escape(val));
	}

	CsvFile& CsvFile::endl(CsvFile& file)
	{
		file.EndRow();
		return file;
	}

	CsvFile& CsvFile::Flush(CsvFile& file)
	{
		file.Flush();
		return file;
	}

	std::string CsvFile::Escape(const std::string& val) const
	{
		std::ostringstream result;
		result << '"';
		std::string::size_type to, from = 0u, len = val.length();
		while (from < len &&
			std::string::npos != (to = val.find_first_of(specialChars, from)))
		{
			result << val.substr(from, to - from) << escapeSeq << val[to];
			from = to + 1;
		}
		result << val.substr(from) << '"';
		return result.str();
	}
}
