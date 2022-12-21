#include <iostream>
#include <boost/stacktrace/stacktrace.hpp>
#include <ranges>
#include <format>

#include "Arguments/Arguments.hpp"
#include "StdIO/StdIO.hpp"
#include "Enumerable/Enumerable.hpp"
#include "Cryptography.h"

#include "fucker.h"

constexpr uint64_t Pow(const uint64_t a, const uint64_t b)
{
    if (b == 1) return a;
    return a * Pow(a, b - 1);
}

bool AddBaseN(uint8_t* c, const uint8_t* const a, const uint8_t* const b, const uint64_t n, const uint64_t base)
{
    int carry = 0;
    for (int i = n - 1; i >= 0; i--)
    {
        int curr = carry + a[i] + b[i];
        carry = curr / base;
        curr %= base;
        c[i] = curr;
    }
    //LogDebug("AddBaseN: a=[|",
    //    [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(a[i]) + ";"); return buf; }(),
    //    "|], b=[|",
    //    [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(b[i]) + ";"); return buf; }(),
    //    "|], n=", Convert::ToString(n), ", base=", Convert::ToString(base));
    //LogDebug("AddBaseN: c=[|", [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(c[i]) + ";"); return buf; }(), "|]");
    if (carry > 0) return false;
    return true;
}

bool AddUint64BaseN(uint8_t* c, const uint8_t* const a, uint64_t b, const uint64_t n, const uint64_t base)
{
    //LogDebug("AddUint64BaseN: a=[|",
    //    [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(a[i]) + ";"); return buf; }(),
    //    "|], b=",
    //    Convert::ToString(b), ", n=", Convert::ToString(n), ", base=", Convert::ToString(base));
    auto* nb = new uint8_t[n];
    for (int i = n - 1; i >= 0; --i)
    {
        nb[i] = b % base;
        b /= base;
    }
    //nb[0] = b;
    if (b > 0) return false;
    //LogDebug("AddUint64BaseN: nb=[|", [&]() { std::string buf{}; for (auto i = 0; i < n; ++i) buf.append(Convert::ToString(nb[i]) + ";"); return buf; }(), "|]");
    const auto succ = AddBaseN(c, a, nb, n, base);
    delete[] nb;
    return succ;
}

std::optional<std::string> PasswordAdd(const std::string& password, const uint64_t offset, const std::string& alphabet)
{
    std::string a(password.length(), '\0');
    std::string c(password.length(), '\0');
    std::transform(password.begin(), password.end(), a.begin(), [&](const char x) { return static_cast<char>(alphabet.find(x)); });
    const auto succ = AddUint64BaseN((uint8_t*)c.data(), (uint8_t*)a.data(), offset, password.length(), alphabet.length());
    if (!succ) return std::nullopt;
    std::transform(c.begin(), c.end(), c.begin(), [&](const char x) { return alphabet[x]; });
    //LogDebug("PasswordAdd: ", password, " + ", Convert::ToString(offset), " = ", c, " base ", alphabet);
    return { c };
}


#if defined(_DEBUG)
int WINAPI WinMain(
    _In_ HINSTANCE hInstance,
    _In_opt_ HINSTANCE hPrevInstance,
    _In_ LPSTR lpCmdLine,
    _In_ int nShowCmd)
#else
int main(int argc, const char** argv)
#endif
{
    Args::Argument<std::string> prefixArg
    {
        "-p",
        "前缀",
        ""
    };

    Args::Argument<std::string> suffixArg
    {
        "-s",
        "后缀",
        ""
    };

    Args::Argument<size_t, 1, true> dynamicArg
    {
        "-d",
        "动态字符个数"
    };

    Args::Argument<std::string, 1, true> alphabetArg
    {
        "-a",
        "字母表"
    };
    Args::Argument<bool, 0> cpuArg
    {
        "-cpu",
        "仅使用cpu",
        false,
        [](auto) { return true; }
    };

    Args::Arguments args{};
    args.Add(prefixArg, suffixArg, dynamicArg, alphabetArg, cpuArg);

    try
    {
#if defined(_DEBUG)
        const std::string prefix = "[[[[";
        const std::string suffix = "]]]]";
        const uint64_t dynamic = 4;
        const std::string alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789/@_+";
        const auto onlyCpu = false;
#else
        args.Parse(argc, argv);

        const auto prefix = args.Value(prefixArg);
        const auto suffix = args.Value(suffixArg);
        const auto dynamic = args.Value(dynamicArg);
        const auto alphabet = args.Value(alphabetArg);
        const auto onlyCpu = args.Value(cpuArg);
#endif


        std::ostream::sync_with_stdio(false);

        const auto maxOffset = Pow(alphabet.length(), dynamic);

        if (onlyCpu)
        {
            for (const auto i : std::views::iota(uint64_t{ 0 }, maxOffset))
	        {
                Cryptography::Md5 md5{};
                const auto key = std::format("{}{}{}", prefix, PasswordAdd(std::string(dynamic, alphabet[0]), i, alphabet).value(), suffix);
                md5.Append((uint8_t*)key.data(), key.length());
                std::cout << key << " " << (std::string)md5.Digest() << "\n";
	        }
        }
        else
        {
            constexpr uint64_t roundSize = 2000;

            for (uint64_t i = 0; i < maxOffset; i += roundSize)
            {
                const auto  num = i + roundSize >= maxOffset ? (maxOffset - i) : roundSize;
                auto d = FuckingCall({ prefix.data(), prefix.length() }, { suffix.data(), suffix.length() }, dynamic, { alphabet.data(), alphabet.size() }, i, num);
                std::cout << std::string_view(d.Data, d.Size);
                FuckingFree(d);
            }
        }
    }
    catch (const std::exception& ex)
    {
        SetForegroundColor(Console::Color::Red);
        Console::Error::WriteLine("ERROR!!!");
        Console::Error::WriteLine(args.GetValuesDesc());
        Console::Error::WriteLine(ex.what());
        Console::Error::WriteLine(boost::stacktrace::stacktrace());

        SetForegroundColor(Console::Color::White);
        Console::WriteLine("Usage:");
        Console::WriteLine(args.GetDesc());
        system("pause");
    }
}
