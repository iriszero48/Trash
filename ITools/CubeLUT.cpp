#include "CubeLUT.h"

#include <sstream>

namespace __Detail
{
	template<class... T> struct Visitor : T... { using T::operator()...; };
}

namespace Lut
{
    std::string CubeLut::ReadLine(std::ifstream& infile, const char lineSeparator)
    {
        constexpr char commentMarker = '#';
        std::string textLine{};
        while (textLine.empty() || textLine[0] == commentMarker)
        {
            if (infile.eof()) { status = LutState::PrematureEndOfFile; break; }
            getline(infile, textLine, lineSeparator);
            if (infile.fail()) { status = LutState::ReadError; break; }
        }
        return textLine;
    }

    CubeLut::Row CubeLut::ParseTableRow(const std::string& lineOfText)
    {
        constexpr int n = 3;
        float f[n];
        std::istringstream line(lineOfText);
        for (float& i : f)
        {
            line >> i;
            if (line.fail())
            {
	            status = LutState::CouldNotParseTableData;
            	break;
            }
        }
        return Row(f);
    }

    const CubeLut::TableType& CubeLut::GetTable() const
    {
        return table;
    }

	CubeLut::Dim CubeLut::GetDim() const
    {
        return std::visit([&]<typename T0>(const T0& tb)
        {
            using T = std::decay_t<T0>;
            if constexpr (std::is_same_v<T, Table1D>)
            {
                return Dim::_1D;
            }
        	if constexpr (std::is_same_v<T, Table3D>)
            {
                return Dim::_3D;
            }
            else
            {
                throw std::runtime_error("non-exhaustive visitor!");
            }
        }, table);
    }

    uint64_t CubeLut::Length() const
    {
        return std::visit(__Detail::Visitor
        {
            [](const Lut::Table1D& t) { return t.Length(); },
            [](const Lut::Table3D& t) { return t.Length(); }
        }, table);
    }

    CubeLut::LutState   CubeLut::LoadCubeFile(std::ifstream& infile)
    {
        status = LutState::OK;
        Title.clear();
        DomainMin = Row(0.0);
        DomainMax = Row(1.0);

        constexpr char newlineCharacter = '\n';
        char lineSeparator = newlineCharacter;

        for (int i = 0; i < 255; i++)
        {
	        const char inc = infile.get();
            if (inc == newlineCharacter) break;
            if (constexpr char carriageReturnCharacter = '\r'; inc == carriageReturnCharacter)
            {
                if (infile.get() == newlineCharacter) break;
                lineSeparator = carriageReturnCharacter;
                break;
            }
            if (i > 250)
            {
	            status = LutState::LineError;
            	break;
            }
        }
        infile.seekg(0);
        infile.clear();

        int cntTitle, cntSize, cntMin, cntMax;
        int n = cntTitle = cntSize = cntMin = cntMax = 0;

        while (status == LutState::OK)
        {
	        const auto linePos = infile.tellg();
            std::string lineOfText = ReadLine(infile, lineSeparator);
            if (status != LutState::OK) break;

            std::istringstream line(lineOfText);
            std::string keyword;
            line >> keyword;

            if ("+" < keyword && keyword < ":")
            {
                infile.seekg(linePos);
                break;
            }

            if (keyword == "TITLE" && cntTitle++ == 0) {
	            constexpr char quote = '"';
	            char startOfTitle;
	            line >> startOfTitle;
	            if (startOfTitle != quote) { status = LutState::TitleMissingQuote; break; }
	            getline(line, Title, quote);
            }
            else if (keyword == "DOMAIN_MIN" && cntMin++ == 0)
            {
                float domainMin[3];
	            line >> domainMin[0] >> domainMin[1] >> domainMin[2];
                DomainMin = Row(domainMin);
            }
            else if (keyword == "DOMAIN_MAX" && cntMax++ == 0)
            {
                float domainMax[3];
	            line >> domainMax[0] >> domainMax[1] >> domainMax[2];
                DomainMax = Row(domainMax);
            }
            else if (keyword == "LUT_1D_SIZE" && cntSize++ == 0)
            {
	            line >> n;
	            if (n < 2 || n > 65536) { status = LutState::LUTSizeOutOfRange; break; }
                table = Table1D(n);
            }
            else if (keyword == "LUT_3D_SIZE" && cntSize++ == 0)
            {
	            line >> n;
	            if (n < 2 || n > 256) { status = LutState::LUTSizeOutOfRange; break; }
                table = Table3D(n);
            }
            else
            {
	            status = LutState::UnknownOrRepeatedKeyword;
            	break;
            }

            if (line.fail())
            {
	            status = LutState::ReadError;
            	break;
            }
        }

        if (status == LutState::OK && cntSize == 0) status = LutState::LUTSizeOutOfRange;

        if (status == LutState::OK && (DomainMin.B >= DomainMax.B || DomainMin.G >= DomainMax.G
            || DomainMin.R >= DomainMax.R))
            status = LutState::DomainBoundsReversed;

        std::visit([&]<typename T0>(T0& tb)
            {
	            using T = std::decay_t<T0>;
	            if constexpr (std::is_same_v<T, Table1D>)
	            {
                    for (int i = 0; i < tb.Length() && status == LutState::OK; i++)
                    {
                        tb.At(i) = ParseTableRow(ReadLine(infile, lineSeparator));
                    }
	            }
	            else if constexpr (std::is_same_v<T, Table3D>)
	            {
                    for (int b = 0; b < n && status == LutState::OK; b++)
                    {
                        for (int g = 0; g < n && status == LutState::OK; g++)
                        {
                            for (int r = 0; r < n && status == LutState::OK; r++)
                            {
                                tb.At(r, g, b) = ParseTableRow(ReadLine(infile, lineSeparator));
                            }
                        }
                    }
	            }
	            else
	            {
                    throw std::runtime_error("non-exhaustive visitor!");
	            }
            }, table);
        return status;
    }

    CubeLut::LutState   CubeLut::SaveCubeFile(std::ofstream& outfile)
    {
        if (status != LutState::OK) return status;

        constexpr char space = ' ';
        constexpr char quote = '"';

        if (!Title.empty()) outfile << "TITLE" << space << quote << Title << quote << std::endl;
        outfile << "# Created by CubeLUT.cpp" << std::endl;
        outfile << "DOMAIN_MIN" << space << DomainMin.R << space << DomainMin.G
            << space << DomainMin.B << std::endl;
        outfile << "DOMAIN_MAX" << space << DomainMax.R << space << DomainMax.G
            << space << DomainMax.B << std::endl;

        std::visit([&]<typename T0>(const T0& tb)
        {
            using T = std::decay_t<T0>;
            if constexpr (std::is_same_v<T, Table1D>)
            {
                const auto n = tb.Length();
                outfile << "LUT_1D_SIZE" << space << n << std::endl;
                for (int i = 0; i < n && outfile.good(); i++)
                {
                    const auto [r, g, b] = tb.At(i);
                    outfile << r << space << g << space << b << std::endl;
                }
            }
            else if constexpr (std::is_same_v<T, Table3D>)
            {
            	const auto n = tb.Length();
                outfile << "LUT_3D_SIZE" << space << n << std::endl;

                for (int b = 0; b < n && outfile.good(); b++)
                {
                    for (int g = 0; g < n && outfile.good(); g++)
                    {
                        for (int r = 0; r < n && outfile.good(); r++)
                        {
                            const auto [rv, gv, bv] = tb.At(r, g, b);
                            outfile << rv << space << gv << space << bv << std::endl;
                        }
                    }
                }
            }
            else
            {
                throw std::runtime_error("non-exhaustive visitor!");
            }
        }, table);

        outfile.flush();
        return (outfile.good() ? LutState::OK : LutState::WriteError);
    }

    CubeLut CubeLut::FromCubeFile(const std::filesystem::path& file)
    {
        CubeLut cube;
        enum { OK = 0, ErrorOpenInFile = 100, ErrorOpenOutFile };

        std::ifstream infile(file);
        if (!infile)
        {
            throw std::runtime_error("Could not open input file");
        }
        const auto ret = cube.LoadCubeFile(infile);

        infile.close();
        if (ret != LutState::OK)
        {
            throw std::runtime_error("Could not parse the cube info in the input file.");
        }
        return cube;
    }
}
