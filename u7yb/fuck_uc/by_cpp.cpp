#include <filesystem>
#include <optional>
#include <unordered_map>
#include <iostream>

#include "String.h"

constexpr auto RootPath = "36,)+Lk_uQLj[[GH[KeI9}SM[MKt-VAnA)e;;4%09_9IQ34Rg_GwPK`";
constexpr auto DonePath = "36,)+Lk_uQLj[[GH[KeI9}SM[MKt-VAnA)e;;4%09_9IQ34Rg_Gus9XHO";

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
"0uPqDcO7UWrk}ZfTW57M-CsF;MKjb6sLUJ@`k^a3SF7BlsOpmheAd8{VXD%RsMlHT19Gh"
"0uK$XB{4,BroxR{23O0).V)Wvs)xT3ewpRaYAJ+[sN`D$dzZ%i6zh"
"0uPt`e'nRWsN8qYU7S5i-Cr2}sk^TaZ`dnCfF)CjsN`D$dzZ%i6zh"
"0uPnDc0-R`sN`D$dzZ%l-CsF;MKjb6s,CDA`0z1STzpV5-}8Zh"
"0uPhjQhVE3s,6`Id{{zy-Cw{7fbL&{rSo(+dD5[wVtzl8rjd~aVO2MhaqhUasMlHT19Gh"
"0uPk.ckN'lsO^adSw^fU-Cq!a6zh"
"0uPnEar&Sls,Jmk`1]`n-Cw{9ar&Sls,Jmk`1dY=Vti,,sOpmheA]=z6zh"
"0uPhfT_KMJrSJ06e}N]G-Cwu^QLhC%rSJ,1e^-EyT{1rn-}8Zh"
"0uPhh]~layrSozJVlPDK-CwxcQLMN+rSozJVlW7G]~laysOpmheA]=z6zh"
"0uPklb3Gumrn$OMd_H!l-CsF;MKjb6rp;yh[Cu``b2qO_sPu$G`0s1d6zh"
"0uPk[^_VWMsK~j1d~0h(-CwxS^_VWMsK~j1d~7h+Vtzl8s19bB[YmA@6zh"
"0uPt-T_Sa8sL^Q!_19(J-Cx!fVtzl8rlhA7e#lOcT_Sa8sL^Q!_1D%HVX_@n-}8Zh"
"0uPhfeI8_xrm'4.VN}N=-CwxE[I52wrSKpiZGcos^C,LFsN`D$dzZ%i6zh"
"0uPhhe'vr2s)xm8d~0h(-Cwu`e'vr2s)xm8d~7gqTzpV5-}8Zh"
"0uPn=ZLP=Yt('rKUpuxU-Cw{3ZLP=Yt('rKUp}rRWqvA3sN`D$dzZ%i6zh"
"0uPk7Yk]eOsNk'_XiGyn-CwxJYknx9s)xT1USQ}6]FHTmt(QlEdzk,QQhVE!sm7aY19Gh"
"0uPk_V9)z&t&j'aY}h-C-CwxVV9)z&t&j'aY}o-.Y6M)Hh"
"0uPkIfFAF1rjo$#Wm2-r-CwxAfFAF1rjo$#Wm9-tVtzl8-}8Zh"
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
