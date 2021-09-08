#include <filesystem>
#include <optional>
#include <unordered_map>
#include <iostream>

#include "String.h"

constexpr auto RootPath = "36,)+Lk_uQLj[[GH[KeI9}SM[MKt-VAnA)e;;4%09_9IQ34Rg_GwPK`h";
constexpr auto DonePath = "36,)+Lk_uQLj[[GH[KeI9}SM[MKt-VAnA)e;;4%09_9IQ34Rg_Gus9XHOh";

static const auto GetKid = [](const std::filesystem::path& fullpath) -> std::optional<std::wstring>
{
    const auto path = fullpath.stem().wstring();
    if (path[0] == L'[')
    {
        return path.substr(1, path.find(L']') - 1);
    }
    return std::nullopt;
};

const auto KindMap = std::unordered_map<std::wstring, std::wstring>
{
"O^E`OWl[^[VXKifrjnh]23P^4H=kY0H[c-#cO7UWrk}ZfTW@7EeIFm~s19b=eB-WfQRnSvh"
"O^E^U@qS8YTzpV5-}Vr4.#!]EXh3''d,)NmsN}#dK3-uDTzpV5.)5!h"
"O^E`Pa1LgkZ,0[erk%jx23P^49Y4n}e'nRWsN8qYU7Z5UTzpV5.)5!h"
"O^E`NX,ng[YOSU.roxR{23P^4H=kY0H[c)#c0-R`sN`D$dzZ&h20h"
"O^E`LeYn1YYkerSrox{-23P^4s,6`Id}&qwQhVE3sPu$G`0z)-U[O;frkFa1dzk,QQRnSvh"
"32&bN.6DUmrSfhGb-R-Y`!3+B@Osc220h"
"O^E`NXLe^FeI8L;sN};#23P^4s,L;@Xj5JNSbjg5rkEWkdAh}UeIFm~.)5!h"
"O^E`LdBRGc[eL^Gsm7aY23P^4rSIi+Wkr+_T_KMJrSJ06e}N^C20h"
"O^E`Ld}vNRTzgVErkWwZ23P^4rp00-dzk!kYk[c!rS^2RV.U50eIFm~.)5!h"
"O^E`Me{9owbnZ0rrSSFz23P^4H=kY0H[c&Pb3Gumrn$OMd_O!nVtzl8.)5!h"
"O^E`M`m}9Y]%&RDsPYX=23P^4rnWLI[^AW3`tu@vsPu$G`0z-eVX_@n.)5!h"
"O^E`PQ^zXj_[[W;sj{#]23P^4sPu$G`0z)E_@Aj.sfm_X]wLVIYO][#s19bB[YmB=20h"
"O^E`LdGfuve'nX^rkNON23P^4rm'4.VO'E5eI8_xro^+QR}%9KTzpV5.)5!h"
"O^E`Le#;%dU[X'6rS^9823P^4rS^yjT5oaYfbB~)sN`D$dzZ&h20h"
"O^E`NVPaSG]FYt]s+X=f23P^4s+k-zc&knRfF4OWroxeEWk6kKTzpV5.)5!h"
"O^E`MUSQ}6]FHTmt(Qj%23P^4rmZG1Wk6h$d+q]CrnaFG_VpEdc0#zesMlJmdB[Yiekys]h"
"O^E`MahPu{d,=c(t(uKV23P^4rnro0Qciu!Sb}AgsN}!xP7Wh"
"O^E`MZ&m(ISbVKtsLUZ'23P^4rl`!PT545T_@@K_sPu$G`0s2c20h"
};

static const auto Move = [](const std::filesystem::path& src, std::filesystem::path dest, const std::wstring& kid)
{
    std::cout << src.stem() << " -> " << dest.stem() << std::endl;
    if (kid.empty()) std::cout << "(!!!) ";
    std::string input;
    std::getline(std::cin, input);
    if (!input.empty())
    {
        dest.replace_filename(String::FormatWstr(
            kid.empty() ? "{}{}{}" : "{}_{}{}",
            input, kid, dest.extension().wstring()));
    }
    if (std::filesystem::exists(dest))
    {
        std::cerr << "exists!" << std::endl;
        exit(EXIT_FAILURE);
    }
    std::filesystem::rename(src, dest);
    std::cout << ">>> " << dest.filename() << std::endl;
};

int main(int argc, char const *argv[])
{
    for (const auto& file: std::filesystem::recursive_directory_iterator(RootPath))
    {
        if (file.is_regular_file() && file.path().filename().wstring().at(0) != L'.')
        {
            if (const auto kid = GetKid(file.path()); kid.has_value() && KindMap.find(*kid) != KindMap.end())
            {
                const auto newKid = KindMap.at(*kid);
                const auto f = String::FormatWstr("{}_{}{}",
                    file.path().stem().wstring().substr(2 + kid->length()),
                    newKid,
                    file.path().extension().wstring());
                const auto src = file.path();
                const auto dest = std::filesystem::path(DonePath) / f;
                Move(src, dest, newKid);
            }
            else
            {
                const auto src = file.path();
                const auto dest = DonePath / file.path().filename();
                Move(src, dest, L"");
            }
        }
    }
}
