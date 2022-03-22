#include <vector>
#include <cstdint>
#include <unordered_map>
#include <future>
#include "Function.h"

#include "Convert.h"
#include "Seq.h"
#include "String.h"

#include <iostream>

namespace Path85
{
    namespace __Detail
    {
        //static const auto Table = R"(!#$%&'()+,-.0123456789;=@ABCDEFGHIJKLMNOPQRSTUVWXYZ[]^_`abcdefghijklmnopqrstuvwxyz{}~)";
        static const auto Table = [](){
            static const auto toStringOpt = [](const char x) { return std::make_optional(std::string(1, x)); };
            static const auto range = [](const auto x) { return Seq::List<uint64_t>::Init(x, [](const auto x) { return x; }); };
            static const auto plus = [](const auto x) { return std::bind(std::plus<>{}, std::placeholders::_1, static_cast<uint64_t>(x)); };
            return Seq::List<std::string>::Init(7, [](const int i) {
                return std::async(std::unordered_map<int, std::function<Seq::List<std::string>()>>
                {
                    { 0, [](){ return Seq::List<char>(std::string_view("!#$%&'()+,-.")).Choose(toStringOpt); } },
                    { 1, [](){ return range(10).Choose([](const auto x) { return Convert::ToString(x); }); } },
                    { 2, [](){ return Seq::List<char>(std::string_view(";=@")).Choose(toStringOpt); } },
                    { 3, [](){ return range(26).Map(plus('A')).Map([](const auto x){ return static_cast<char>(x); }).Choose(toStringOpt); } },
                    { 4, [](){ return Seq::List<char>(std::string_view("[]^_`")).Choose(toStringOpt); } },
                    { 5, [](){ return range(26).Map(plus('a')).Map([](const auto x){ return static_cast<char>(x); }).Choose(toStringOpt); } },
                    { 6, [](){ return Seq::List<char>(std::string_view("{}~")).Choose(toStringOpt); } }
                }.at(i)).get().Concat("");
            }).Concat("");
        }();

        uint64_t Pow(uint64_t base, uint64_t exp)
        {
            if (exp == 0) return 1;
            return Seq::List<uint64_t>::Create(exp, base).Reduce(std::multiplies<>{});
        }
    }

    std::string Encode(const std::string_view& data)
    {
        const auto block = Seq::List<uint8_t>(data).ChunkBySize(4);
        const auto lastLen = block.rev().Head().Length();
        const auto res = block.Map([](const auto& x){return [=]() { return
            x.Length() == 4 ? x : x.Append(Seq::List<uint8_t>::Create(4 - x.Length(), 0)); }()
            .Mapi([](const auto i, const auto& x){ return static_cast<uint32_t>(x) << ((3 - i) * 8); })
        .Reduce(std::bit_or<>{}); })
        .Map([](const uint32_t x){ return Seq::List<uint32_t>::Init(5, [=](const auto i){ return x / __Detail::Pow(85, 4 - i) % 85; }); })
        .Reduce([](const auto s, const auto x){ return s.Append(x); })
        .Map([](const auto x) { return std::string(1, Seq::List<char>(std::string_view(__Detail::Table)).Item(x)); })
        .Concat("");
        return res.substr(0, res.length() - (4 - lastLen));
    }

    std::string Decode(const std::string& data)
    {
        const auto lastLen = data.length() % 5 == 0 ? 5 : data.length() % 5;
        const auto padLen = data.length() / 5 * 5 + (lastLen == 5 ? 0 : 5);
        return Seq::List<char>(String::PadRight(data, padLen, '~'))
            .ChunkBySize(5)
            .Map([](const Seq::List<char>& x) { return x.Map([](const auto x) { return Seq::List<char>(__Detail::Table).IndexOf(x); }); })
            .Map([](const Seq::List<uint64_t>& x) { return x.Mapi([](const auto i, const auto x) { return x * __Detail::Pow(85, 4 - i); }).Sum(); })
            .Map([](const uint64_t x) { return Seq::List<uint32_t>::Init(4, [=](const auto i) { return x >> static_cast<uint8_t>((3 - i) * 8) & 0xFF; }); })
            .Reduce([](const auto s, const auto x){ return s.Append(x); })
            .Take((padLen / 5) * 4 - (5 - lastLen))
            .Map([](const auto x) { return std::string(1, x); })
            .Concat("");
    }
}
