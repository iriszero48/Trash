#pragma region Header
// std
#include <filesystem>
#include <iostream>
#include <ranges>
#include <execution>
#include <any>
#include <chrono>
#include <queue>
#include <atomic>
#include <mutex>
#include <unordered_set>

//WinAPI
#include <Windows.h>
#include <CommCtrl.h>
#include <Shlwapi.h>
#include <ShlObj.h>

// 3rd
#include <nlohmann/json.hpp>
#include <imgui.h>
#include <wrl.h>
#include <imgui_impl_win32.h>
#include <imgui_impl_dx11.h>

// dx11
#include <d3d11.h>
#include <d3dcompiler.h>

// utility
#include "CubeLUT.h"
#include "Enumerable.h"
#include "String.h"
#include "Log.h"
#include "StdIO.h"
#include "Time.h"
#include "Cryptography.h"
#include "Enum.h"
#include "File.h"

// project
#include "ImageTools.h"
#include "Text.h"

// res
#include "RHR.h"

// shader
#include "Shader_GenerateNormalTexture.h"
#include "Shader_LinearDodgeColor.h"
#include "Shader_LUT3D.h"
#include "Shader_NormalMapConvertorRGB2DA.h"
#include "Shader_LinearDodgeImage.h"
#pragma endregion Header

#pragma region Type
// dx11
using Dx11DevType = ID3D11Device;
using Dx11DevCtxType = ID3D11DeviceContext;
#pragma endregion Type

#pragma region Constexpr
constexpr uint32_t MaxPathLengthW = 32767;
constexpr uint32_t MaxPathLength8 = MaxPathLengthW * 3;
#pragma endregion Constexpr

#pragma region Var
// dx11-imgui
static Microsoft::WRL::ComPtr<Dx11DevType> D3D11Dev = nullptr;
static Microsoft::WRL::ComPtr<Dx11DevCtxType> D3D11DevCtx = nullptr;
static Microsoft::WRL::ComPtr<IDXGISwapChain> D3D11SwapChain = nullptr;
static Microsoft::WRL::ComPtr<ID3D11RenderTargetView> D3D11MainRenderTargetView = nullptr;

// dx11-cs
static Microsoft::WRL::ComPtr<Dx11DevType> D3D11CSDev = nullptr;
static Microsoft::WRL::ComPtr<Dx11DevCtxType> D3D11CSDevCtx = nullptr;

// proc status
static bool IsProcessing = false;
static std::jthread ProcThread;
#pragma endregion Var

#pragma region Extern
extern IMGUI_IMPL_API LRESULT ImGui_ImplWin32_WndProcHandler(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam);
#pragma endregion Extern

#pragma region Helper
Enum(Processor, CPU, GPU);
Enum(ImageFormat, jpg, png, bmp, tga);

template<class... T> struct Visitor : T... { using T::operator()...; };

struct U8String
{
    std::size_t BufLength = 0;
    std::u8string Buf = std::u8string(MaxPathLength8, 0);

    U8String() = default;

	U8String(const std::u8string& buf)
    {
        std::copy_n(buf.begin(), buf.length(), Buf.begin());
        BufLength = buf.length();
    }

    U8String& operator=(const U8String& str)
    {
        Set(str.GetView());
        return *this;
    }

    char* GetAddress() { return reinterpret_cast<char*>(Buf.data()); }
    [[nodiscard]] std::filesystem::path GetPath() const { return std::u8string_view(Buf.data(), BufLength); }
    [[nodiscard]] std::u8string_view GetView() const { return { Buf.data(), BufLength }; }
    [[nodiscard]] std::size_t Length() const { return BufLength; }
    [[nodiscard]] bool Empty() const { return BufLength == 0; }

    void Update()
    {
        BufLength = std::ranges::find(Buf, 0) - Buf.begin();
    }

    void Set(const std::u8string_view& buf)
    {
        std::copy_n(buf.begin(), buf.length(), Buf.begin());
        Buf[buf.length()] = 0;
        BufLength = buf.length();
    }

    void Clear()
    {
        BufLength = 0;
        Buf[0] = 0;
    }

    static friend bool operator==(const U8String& s1, const U8String& s2)
    {
        if (s1.BufLength != s2.BufLength) return false;
        return std::u8string_view(s1.Buf).substr(0, s1.BufLength) == std::u8string_view(s2.Buf).substr(0, s1.BufLength);
    }

    static int ImGuiInputTextCallback(ImGuiInputTextCallbackData* data)
    {
        if (data->EventFlag == ImGuiInputTextFlags_CallbackResize)
        {
            static_cast<U8String*>(data->UserData)->Update();
        }
        return 0;
    }
};

struct ImageView
{
    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> SRV;
    int Width;
    int Height;

    ImageView() : SRV(nullptr), Width(0), Height(0) {}
    ImageView(const Microsoft::WRL::ComPtr<ID3D11ShaderResourceView>& srv, const int w, const int h) : Width(w), Height(h)
    {
        srv.CopyTo(SRV.GetAddressOf());
    }
    ImageView(const ImageView& iv, const Microsoft::WRL::ComPtr<ID3D11ShaderResourceView>& srv) : Width(iv.Width), Height(iv.Height)
    {
        srv.CopyTo(SRV.GetAddressOf());
    }
    ImageView(const ImageView& iv) : Width(iv.Width), Height(iv.Height)
    {
        iv.SRV.CopyTo(SRV.GetAddressOf());
    }
    ImageView(ImageView&& iv) noexcept : SRV(std::move(iv.SRV)), Width(iv.Width), Height(iv.Height) {}

    ImageView& operator=(const ImageView& iv)
    {
        Width = iv.Width;
        Height = iv.Height;
        iv.SRV.CopyTo(SRV.ReleaseAndGetAddressOf());
        return *this;
    }

    ImageView& operator=(ImageView&& iv) noexcept
    {
        Width = iv.Width;
        Height = iv.Height;
        SRV = std::move(iv.SRV);
        return *this;
    }
};

std::filesystem::path GetAppData()
{
    wchar_t* buf;
    SHGetKnownFolderPath(FOLDERID_LocalAppData, KF_FLAG_CREATE, nullptr, &buf);
    return buf;
}

bool IsFilePath(const U8String& path)
{
    return std::filesystem::is_regular_file(path.GetView());
}

bool IsFolderPath(const U8String& path)
{
    return std::filesystem::is_directory(path.GetView());
}

bool IsExist(const U8String& path)
{
    return std::filesystem::exists(path.GetView());
}

std::vector<std::filesystem::path> GetFiles(const std::filesystem::path& path)
{
    std::vector<std::filesystem::path> res{};
    for (const auto& p : std::filesystem::directory_iterator(path))
    {
        if (p.is_regular_file()) res.emplace_back(p);
    }
    return res;
}
#pragma endregion Helper

#pragma region Exception
class Exception : public std::runtime_error
{
public:
    template <typename ...Args>
    explicit Exception(Args&&... args) : std::runtime_error(std::format(std::forward<Args>(args)...)) {}
};

#define MakeException(name, base) class name : public base { using base::base; }

MakeException(WinApiException, Exception);
MakeException(D3D11Exception, Exception);
MakeException(ToolException, Exception);
MakeException(LibZipException, Exception);
MakeException(ImgToolsException, Exception);

#undef MakeException

#define Ex(ex, ...) ex("[{}:{}] [{}] [{}] {}", __SourceFileName__, __LINE__, __FUNCTION__, #ex , std::format(__VA_ARGS__))
#pragma endregion Exception

#pragma region Log
struct LogMsg
{
    decltype(std::chrono::system_clock::now()) Time;
    decltype(std::this_thread::get_id()) Id;
    const char* File;
    decltype(__LINE__) Line;
    const char* Func;
    std::wstring Msg;

    static decltype(auto) LogTime(const decltype(LogMsg::Time)& time)
    {
        auto t = std::chrono::system_clock::to_time_t(time);
        tm local{};
        Time::Local(&local, &t);
        return String::FromStream(std::put_time(&local, "%F %X"));
    }

    static void LogExceptionImpl(std::vector<std::string>& out, const std::exception& e, const std::size_t level)
    {
        out.push_back(String::Format("{}{}", std::string(level * 2, ' '), e.what()));
        try
        {
            std::rethrow_if_nested(e);
        }
        catch (const std::exception& ex)
        {
            LogExceptionImpl(out, ex, level + 1);
        }
    }

    static std::string LogException(const std::exception& e)
    {
        std::vector<std::string> out{};
        LogExceptionImpl(out, e, 0);
        return String::Join(out.begin(), out.end(), "\n");
    }

    template <typename T, size_t S>
    static constexpr const char* GetFilename(const T(&str)[S], size_t i = S - 1)
    {
        for (; i > 0; --i) if (str[i] == '/' || str[i] == '\\') return &str[i + 1];
        return str;
    }
};

constexpr auto __SourceFileName__ = LogMsg::GetFilename(__FILE__);

static Logger<LogMsg> Log{};

#define __Log(lv, ...) if (Log.Level >= lv) Log.Write<lv>(std::chrono::system_clock::now(), std::this_thread::get_id(), __SourceFileName__, __LINE__, __FUNCTION__, String::FormatW(__VA_ARGS__))
#define LogNone(...) __Log(LogLevel::None, __VA_ARGS__)
#define LogErr(...) __Log(LogLevel::Error, __VA_ARGS__)
#define LogWarn(...) __Log(LogLevel::Warn, __VA_ARGS__)
#define LogLog(...) __Log(LogLevel::Log, __VA_ARGS__)
#define LogInfo(...) __Log(LogLevel::Info, __VA_ARGS__)
#define LogDebug(...) __Log(LogLevel::Debug, __VA_ARGS__)
#pragma endregion Log

namespace Pick
{
    using HookParam = std::tuple<std::wstring, HWND>;

    std::filesystem::path PickBase(const std::wstring_view& filter = L"Any File\0*\0", const std::wstring_view& title = L"Select...", const LPOFNHOOKPROC hook = nullptr, const DWORD flags = 0)
    {
	    const auto filename = std::make_unique<wchar_t[]>(MaxPathLengthW);

        OPENFILENAME ofn{};
        ofn.lStructSize = sizeof(ofn);
        ofn.hwndOwner = nullptr;
        ofn.lpstrFilter = filter.data();
        ofn.lpstrFile = filename.get();
        ofn.nMaxFile = MaxPathLengthW;
        ofn.lpstrTitle = title.data();
        ofn.lpfnHook = hook;
        ofn.Flags = flags;

        if (GetOpenFileName(&ofn)) return filename.get();
        
#define CaseErr(ce) case ce: throw Ex(WinApiException, "GetOpenFileName: " #ce)
        return [&]()
        {
            switch (CommDlgExtendedError())
            {
            CaseErr(CDERR_DIALOGFAILURE  );
            CaseErr(CDERR_FINDRESFAILURE );
            CaseErr(CDERR_INITIALIZATION );
            CaseErr(CDERR_LOADRESFAILURE );
            CaseErr(CDERR_LOADSTRFAILURE );
            CaseErr(CDERR_LOCKRESFAILURE );
            CaseErr(CDERR_MEMALLOCFAILURE);
            CaseErr(CDERR_MEMLOCKFAILURE );
            CaseErr(CDERR_NOHINSTANCE    );
            CaseErr(CDERR_NOHOOK         );
            CaseErr(CDERR_NOTEMPLATE     );
            CaseErr(CDERR_STRUCTSIZE     );
            CaseErr(FNERR_BUFFERTOOSMALL );
            CaseErr(FNERR_INVALIDFILENAME);
            CaseErr(FNERR_SUBCLASSFAILURE);
            default: return std::filesystem::path{};
            }
        }();
#undef CaseErr
    }

    std::filesystem::path PickFileAndFolder(const std::wstring_view filter = L"Any File\0*\0", const std::wstring_view& title = L"Select...", DWORD flags = OFN_ENABLEHOOK | OFN_EXPLORER | OFN_NOVALIDATE)
    {
        return PickBase(filter, title, [](HWND curWnd, UINT message, WPARAM wParam, LPARAM lParam) -> UINT_PTR
            {
                if (message == WM_NOTIFY)
                {
                    if ((&reinterpret_cast<OFNOTIFY*>(lParam)->hdr)->code == CDN_SELCHANGE)
                    {
	                    const auto parentWnd = GetParent(curWnd);
                        auto param = HookParam(L"FolderView", nullptr);

                        EnumChildWindows(parentWnd, [](HWND childWnd, LPARAM lParam) -> BOOL
                            {
                                auto& [name, wnd] = *reinterpret_cast<HookParam*>(lParam);
                                const auto nameLen = GetWindowTextLength(childWnd);
                                
                                if (static_cast<std::size_t>(nameLen) != name.length()) return true;
                                
                                std::wstring buf(nameLen, 0);
                                GetWindowText(childWnd, buf.data(), nameLen + 1);
                                
                                if (name == buf)
                                {
                                    wnd = childWnd;
                                    return false;
                                }
                                
                                return true;
                            }, reinterpret_cast<LPARAM>(&param));

                        if (auto& [_, viewWnd] = param; std::get<1>(param))
                        {
                            std::wstring res{};
                            
                            std::vector<std::wstring> buffs{};
                            int index = -1;
                            while (-1 != (index = ListView_GetNextItem(viewWnd, index, LVNI_ALL | LVNI_SELECTED)))
                            {
                                std::vector<wchar_t> buf(MaxPathLengthW, 0);
                                ListView_GetItemText(viewWnd, index, 0, buf.data(), buf.size())
                                buffs.emplace_back(buf.data());
                            }
                            
                            if (buffs.empty()) {}
                            else if (buffs.size() == 1) { res = buffs[0]; }
                            else
                            {
                                for (const auto& buf : buffs)
                                {
                                    res.append(1, '\"');
                                    res.append(buf);
                                    res.append(1, '\"');
                                    res.append(1, ' ');
                                }
                            }
                            CommDlg_OpenSave_SetControlText(parentWnd, edt1, res.c_str());
                        }
                    }
                }
                return 0;
            }, flags);
    }

    std::filesystem::path PickFile(const std::wstring_view& filter = L"Any File\0*\0", const std::wstring_view& title = L"Select...", LPOFNHOOKPROC hook = nullptr, DWORD flags = 0)
    {
        return PickBase(filter.data(), title.data(), nullptr, flags);
    }
}

namespace D3D11
{
    template <int Size>
    Microsoft::WRL::ComPtr<ID3D11ComputeShader> CreateComputeShader(Dx11DevType* dev, const BYTE(&data)[Size])
    {
        Microsoft::WRL::ComPtr<ID3D11ComputeShader> shader{};
        if (auto hr = dev->CreateComputeShader(data, Size, nullptr, shader.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateComputeShader: {}", hr);
        return shader;
    }

    Microsoft::WRL::ComPtr<ID3D11ComputeShader> CreateComputeShader(Dx11DevType* dev, const LPCWSTR srcFile, const LPCSTR functionName)
    {
        Microsoft::WRL::ComPtr<ID3D11ComputeShader> shader;

        constexpr DWORD shaderFlags = D3DCOMPILE_ENABLE_STRICTNESS;
        const D3D_SHADER_MACRO defines[] =
        {
            "USE_STRUCTURED_BUFFERS", "1",
            nullptr, nullptr
        };

        const LPCSTR profile = (dev->GetFeatureLevel() >= D3D_FEATURE_LEVEL_11_0) ? "cs_5_0" : "cs_4_0";

        Microsoft::WRL::ComPtr<ID3DBlob> errorBlob = nullptr;
        Microsoft::WRL::ComPtr<ID3DBlob> blob = nullptr;

        HRESULT hr = D3DCompileFromFile(srcFile, defines, nullptr, functionName, profile,
            shaderFlags, NULL, blob.GetAddressOf(), errorBlob.GetAddressOf());
        if (FAILED(hr)) if (errorBlob) throw Ex(D3D11Exception, "D3DCompileFromFile: {}", std::string_view(static_cast<char*>(errorBlob->GetBufferPointer())));

        hr = dev->CreateComputeShader(blob->GetBufferPointer(), blob->GetBufferSize(), nullptr, shader.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "CreateComputeShader: {}", hr);

        return shader;
    }

    Microsoft::WRL::ComPtr<ID3D11Buffer> CreateStructuredBuffer(Dx11DevType* dev, const UINT elementSize, const UINT count, void* data)
    {
        Microsoft::WRL::ComPtr<ID3D11Buffer> buf;

        D3D11_BUFFER_DESC desc;
        ZeroMemory(&desc, sizeof(D3D11_BUFFER_DESC));
        desc.BindFlags = D3D11_BIND_UNORDERED_ACCESS | D3D11_BIND_SHADER_RESOURCE;
        desc.ByteWidth = elementSize * count;
        desc.MiscFlags = D3D11_RESOURCE_MISC_BUFFER_STRUCTURED;
        desc.StructureByteStride = elementSize;

        HRESULT hr;
        if (data)
        {
            D3D11_SUBRESOURCE_DATA initData{};
            initData.pSysMem = data;
            hr = dev->CreateBuffer(&desc, &initData, buf.GetAddressOf());
        }
        else
            hr = dev->CreateBuffer(&desc, nullptr, buf.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "CreateBuffer: {}", hr);

        return buf;
    }

    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> CreateBufferSRV(Dx11DevType* dev, ID3D11Buffer* buffer)
    {
        Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> srv;

        D3D11_BUFFER_DESC descBuf;
        ZeroMemory(&descBuf, sizeof(descBuf));
        buffer->GetDesc(&descBuf);

        D3D11_SHADER_RESOURCE_VIEW_DESC desc;
        ZeroMemory(&desc, sizeof(desc));
        desc.ViewDimension = D3D11_SRV_DIMENSION_BUFFEREX;
        desc.BufferEx.FirstElement = 0;

        if (descBuf.MiscFlags & D3D11_RESOURCE_MISC_BUFFER_ALLOW_RAW_VIEWS)
        {
            desc.Format = DXGI_FORMAT_R32_TYPELESS;
            desc.BufferEx.Flags = D3D11_BUFFEREX_SRV_FLAG_RAW;
            desc.BufferEx.NumElements = descBuf.ByteWidth / 4;
        }
        else if (descBuf.MiscFlags & D3D11_RESOURCE_MISC_BUFFER_STRUCTURED)
        {
            desc.Format = DXGI_FORMAT_UNKNOWN;
            desc.BufferEx.NumElements = descBuf.ByteWidth / descBuf.StructureByteStride;
        }
        else
        {
            throw Ex(D3D11Exception, "E_INVALIDARG");
        }

        if (const auto hr = dev->CreateShaderResourceView(buffer, &desc, srv.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateShaderResourceView: {}", hr);

        return srv;
    }

    Microsoft::WRL::ComPtr<ID3D11UnorderedAccessView> CreateBufferUAV(Dx11DevType* dev, ID3D11Buffer* buffer)
    {
        Microsoft::WRL::ComPtr<ID3D11UnorderedAccessView> uav;

        D3D11_BUFFER_DESC descBuf;
        ZeroMemory(&descBuf, sizeof(descBuf));
        buffer->GetDesc(&descBuf);

        D3D11_UNORDERED_ACCESS_VIEW_DESC desc;
        ZeroMemory(&desc, sizeof(desc));
        desc.ViewDimension = D3D11_UAV_DIMENSION_BUFFER;
        desc.Buffer.FirstElement = 0;

        if (descBuf.MiscFlags & D3D11_RESOURCE_MISC_BUFFER_ALLOW_RAW_VIEWS)
        {
            desc.Format = DXGI_FORMAT_R32_TYPELESS;
            desc.Buffer.Flags = D3D11_BUFFER_UAV_FLAG_RAW;
            desc.Buffer.NumElements = descBuf.ByteWidth / 4;
        }
        else if (descBuf.MiscFlags & D3D11_RESOURCE_MISC_BUFFER_STRUCTURED)
        {
            desc.Format = DXGI_FORMAT_UNKNOWN;
            desc.Buffer.NumElements = descBuf.ByteWidth / descBuf.StructureByteStride;
        }
        else
        {
            throw Ex(D3D11Exception, "CreateBufferSRV: E_INVALIDARG");
        }

        if (const auto hr = dev->CreateUnorderedAccessView(buffer, &desc, uav.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateBufferUAV: CreateUnorderedAccessView: {}", hr);

        return uav;
    }

    Microsoft::WRL::ComPtr<ID3D11Texture2D> CreateTexture2dUavBuf(Dx11DevType* dev, UINT width, UINT height)
    {
        Microsoft::WRL::ComPtr<ID3D11Texture2D> buf;

        D3D11_TEXTURE2D_DESC texDesc{};
        texDesc.Width = width;
        texDesc.Height = height;
        texDesc.MipLevels = 1;
        texDesc.ArraySize = 1;
        texDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        texDesc.SampleDesc.Count = 1;
        texDesc.SampleDesc.Quality = 0;
        texDesc.Usage = D3D11_USAGE_DEFAULT;
        texDesc.CPUAccessFlags = 0;
        texDesc.BindFlags = D3D11_BIND_SHADER_RESOURCE | D3D11_BIND_UNORDERED_ACCESS;
        texDesc.MiscFlags = 0;

        if (const auto hr = dev->CreateTexture2D(&texDesc, nullptr, buf.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateTexture2D: {}", hr);

        return buf;
    }

    void RunComputeShader(Dx11DevCtxType* devCtx,
                          ID3D11ComputeShader* shader,
                          ID3D11ShaderResourceView** srvs, const UINT srvN,
                          ID3D11UnorderedAccessView** uavs, const UINT uavN,
                          ID3D11SamplerState** sss, const UINT ssN,
                          ID3D11Buffer** cbs, const UINT cbN,
                          const UINT x, const UINT y, const UINT z)
    {
        devCtx->CSSetShader(shader, nullptr, 0);
        devCtx->CSSetShaderResources(0, srvN, srvs);
        devCtx->CSSetUnorderedAccessViews(0, uavN, uavs, nullptr);
        devCtx->CSSetSamplers(0, ssN, sss);
        devCtx->CSSetConstantBuffers(0, cbN, cbs);

        devCtx->Dispatch(x, y, z);

        devCtx->CSSetShader(nullptr, nullptr, 0);
        ID3D11UnorderedAccessView* nullUav[1] = {nullptr};
        devCtx->CSSetUnorderedAccessViews(0, 1, nullUav, nullptr);
        ID3D11ShaderResourceView* nullSrv[1] = {nullptr};
        devCtx->CSSetShaderResources(0, 1, nullSrv);
        ID3D11Buffer* nullCb[1] = {nullptr};
        devCtx->CSSetConstantBuffers(0, 1, nullCb);
        ID3D11SamplerState* nullSs[1] = {nullptr};
        devCtx->CSSetSamplers(0, 1, nullSs);
    }

    template <std::size_t SrvN, std::size_t UavN>
    void RunComputeShader(Dx11DevCtxType* devCtx,
        ID3D11ComputeShader* shader,
        ID3D11ShaderResourceView* (&srvs)[SrvN],
        ID3D11UnorderedAccessView* (&uavs)[UavN],
        const UINT x, const UINT y, const UINT z)
    {
        RunComputeShader(devCtx, shader, srvs, SrvN, uavs, UavN, nullptr, 0, nullptr, 0, x, y, z);
    }

    template <std::size_t SrvN, std::size_t UavN, std::size_t SsN>
    void RunComputeShader(Dx11DevCtxType* devCtx,
        ID3D11ComputeShader* shader,
        ID3D11ShaderResourceView* (&srvs)[SrvN],
        ID3D11UnorderedAccessView* (&uavs)[UavN],
        ID3D11SamplerState* (&sss)[SsN],
        const UINT x, const UINT y, const UINT z)
    {
        RunComputeShader(devCtx, shader, srvs, SrvN, uavs, UavN, sss, SsN, nullptr, 0, x, y, z);
    }

    template <std::size_t SrvN, std::size_t UavN, std::size_t SsN, std::size_t CbN>
    void RunComputeShader(Dx11DevCtxType* devCtx,
        ID3D11ComputeShader* shader,
        ID3D11ShaderResourceView* (&srvs)[SrvN],
        ID3D11UnorderedAccessView* (&uavs)[UavN],
        ID3D11SamplerState* (&sss)[SsN],
        ID3D11Buffer* cbs[CbN],
        const UINT x, const UINT y, const UINT z)
    {
        RunComputeShader(devCtx, shader, srvs, SrvN, uavs, UavN, sss, SsN, cbs, CbN, x, y, z);
    }

    Microsoft::WRL::ComPtr<ID3D11UnorderedAccessView> CreateTexture2dUav(Dx11DevType* dev, ID3D11Texture2D* tex)
    {
        Microsoft::WRL::ComPtr<ID3D11UnorderedAccessView> buf;

        D3D11_UNORDERED_ACCESS_VIEW_DESC uavDesc{};
        uavDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        uavDesc.ViewDimension = D3D11_UAV_DIMENSION_TEXTURE2D;
        uavDesc.Texture2D.MipSlice = 0;

        if (const auto hr = dev->CreateUnorderedAccessView(tex, &uavDesc, buf.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateUnorderedAccessView: {}", hr);

        return buf;
    }

    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> CreateSrvFromTex(Dx11DevType* dev, ID3D11Texture2D* tex)
    {
        Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> buf;

        D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc;
        ZeroMemory(&srvDesc, sizeof(srvDesc));
        srvDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        srvDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
        srvDesc.Texture2D.MipLevels = 1;
        srvDesc.Texture2D.MostDetailedMip = 0;

        if (const auto hr = dev->CreateShaderResourceView(tex, &srvDesc, buf.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateShaderResourceView: {}", hr);

        return buf;
    }

    Microsoft::WRL::ComPtr<ID3D11SamplerState> CreateSampler(Dx11DevType* dev)
    {
        Microsoft::WRL::ComPtr<ID3D11SamplerState> ss;

        D3D11_SAMPLER_DESC desc{};
        ZeroMemory(&desc, sizeof(D3D11_SAMPLER_DESC));
        desc.Filter = D3D11_FILTER_MIN_MAG_MIP_LINEAR;
        desc.AddressU = D3D11_TEXTURE_ADDRESS_CLAMP;
        desc.AddressV = D3D11_TEXTURE_ADDRESS_CLAMP;
        desc.AddressW = D3D11_TEXTURE_ADDRESS_CLAMP;

        if (const auto hr = dev->CreateSamplerState(&desc, ss.GetAddressOf()); FAILED(hr)) throw Ex(D3D11Exception, "CreateSamplerState: {}", hr);

        return ss;
    }

    static void CreateRenderTarget()
    {
        Microsoft::WRL::ComPtr<ID3D11Texture2D> backBuffer;

        auto hr = D3D11SwapChain->GetBuffer(0, IID_PPV_ARGS(backBuffer.GetAddressOf()));
        if (FAILED(hr)) throw Ex(D3D11Exception, "GetBuffer: {}", hr);

        hr = D3D11Dev->CreateRenderTargetView(backBuffer.Get(), NULL, D3D11MainRenderTargetView.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "CreateRenderTargetView: {}", hr);
    }

    static void CreateDeviceD3D(const HWND wnd)
    {
        DXGI_SWAP_CHAIN_DESC sd;
        ZeroMemory(&sd, sizeof(sd));
        sd.BufferCount = 2;
        sd.BufferDesc.Width = 0;
        sd.BufferDesc.Height = 0;
        sd.BufferDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        sd.BufferDesc.RefreshRate.Numerator = 60;
        sd.BufferDesc.RefreshRate.Denominator = 1;
        sd.Flags = DXGI_SWAP_CHAIN_FLAG_ALLOW_MODE_SWITCH;
        sd.BufferUsage = DXGI_USAGE_RENDER_TARGET_OUTPUT;
        sd.OutputWindow = wnd;
        sd.SampleDesc.Count = 1;
        sd.SampleDesc.Quality = 0;
        sd.Windowed = TRUE;
        sd.SwapEffect = DXGI_SWAP_EFFECT_DISCARD;

        UINT createDeviceFlags = D3D11_CREATE_DEVICE_BGRA_SUPPORT;
#if defined(DEBUG) || defined(_DEBUG)
        createDeviceFlags |= D3D11_CREATE_DEVICE_DEBUG;
#endif
        D3D_FEATURE_LEVEL featureLevel;
        constexpr D3D_FEATURE_LEVEL featureLevelArray[2] = { D3D_FEATURE_LEVEL_11_0, D3D_FEATURE_LEVEL_10_0, };
        auto hr = D3D11CreateDeviceAndSwapChain(
	        nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr,
            createDeviceFlags, featureLevelArray, 2, D3D11_SDK_VERSION,
            &sd, D3D11SwapChain.GetAddressOf(),
            D3D11Dev.GetAddressOf(), &featureLevel, D3D11DevCtx.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "D3D11CreateDeviceAndSwapChain: {}", hr);

        hr = D3D11CreateDevice(
            nullptr, D3D_DRIVER_TYPE_HARDWARE, nullptr,
            createDeviceFlags, featureLevelArray, 2, D3D11_SDK_VERSION,
            D3D11CSDev.GetAddressOf(), &featureLevel, D3D11CSDevCtx.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "D3D11CreateDevice: {}", hr);

        CreateRenderTarget();
    }

    static void CleanupRenderTarget()
    {
        D3D11MainRenderTargetView.Reset();
    }

    static void CleanupDeviceD3D()
    {
        CleanupRenderTarget();
        D3D11SwapChain.Reset();
        D3D11DevCtx.Reset();
        D3D11Dev.Reset();

        D3D11CSDevCtx.Reset();
        D3D11CSDev.Reset();
    }

    constexpr int GetThreadGroupNum(const int size)
    {
        constexpr auto groupSize = 32;
        return size / groupSize + (size % groupSize ? 1 : 0);
    }

    Image::ImageFile CreateOutTexture(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& texture)
    {
        Microsoft::WRL::ComPtr<ID3D11Resource> res;

        Microsoft::WRL::ComPtr<ID3D11Texture2D> tex;
        texture.SRV->GetResource(res.GetAddressOf());
        auto hr = res->QueryInterface(tex.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "QueryInterface: {}", hr);

        D3D11_TEXTURE2D_DESC texDesc;
        tex->GetDesc(&texDesc);
        texDesc.Usage = D3D11_USAGE_STAGING;
        texDesc.BindFlags = 0;
        texDesc.CPUAccessFlags = D3D11_CPU_ACCESS_READ;
        texDesc.MiscFlags = 0;

        Microsoft::WRL::ComPtr<ID3D11Texture2D> des;
        hr = dev->CreateTexture2D(&texDesc, nullptr, des.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "CreateTexture2D: {}", hr);

        devCtx->CopyResource(des.Get(), tex.Get());

        D3D11_MAPPED_SUBRESOURCE mapped;
        hr = devCtx->Map(des.Get(), 0, D3D11_MAP_READ, 0, &mapped);
        if (FAILED(hr)) throw Ex(D3D11Exception, "Map: {}", hr);

        constexpr auto comp = 4;
        const int w = mapped.RowPitch / comp;
        const int h = mapped.DepthPitch / mapped.RowPitch;

        Image::ImageFile img(w, h);
        std::copy_n(static_cast<uint8_t*>(mapped.pData), img.Size(), img.Data());

        devCtx->Unmap(des.Get(), 0);

        return img;
    }

    ImageView LoadTextureFromFile(Dx11DevType* dev, const Image::ImageFile& img)
    {
        if (img.Empty()) throw Ex(D3D11Exception, "img.Empty()");

        Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> outSrv;

        D3D11_TEXTURE2D_DESC desc;
        ZeroMemory(&desc, sizeof(desc));
        desc.Width = img.Width();
        desc.Height = img.Height();
        desc.MipLevels = 1;
        desc.ArraySize = 1;
        desc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        desc.SampleDesc.Count = 1;
        desc.Usage = D3D11_USAGE_DEFAULT;
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
        desc.CPUAccessFlags = 0;

        Microsoft::WRL::ComPtr<ID3D11Texture2D> texture;
        D3D11_SUBRESOURCE_DATA subResource{};
        subResource.pSysMem = img.Data();
        subResource.SysMemPitch = desc.Width * 4;
        subResource.SysMemSlicePitch = 0;
        auto hr = dev->CreateTexture2D(&desc, &subResource, texture.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "CreateTexture2D: {}", hr);

        D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc;
        ZeroMemory(&srvDesc, sizeof(srvDesc));
        srvDesc.Format = DXGI_FORMAT_R8G8B8A8_UNORM;
        srvDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE2D;
        srvDesc.Texture2D.MipLevels = desc.MipLevels;
        srvDesc.Texture2D.MostDetailedMip = 0;
        hr = dev->CreateShaderResourceView(texture.Get(), &srvDesc, outSrv.GetAddressOf());
        if (FAILED(hr)) throw Ex(D3D11Exception, "CreateShaderResourceView: {}", hr);

        return { outSrv, img.Width(), img.Height() };
    }

    Microsoft::WRL::ComPtr<ID3D11ShaderResourceView> CreateTexture3d(Dx11DevType* dev, const Lut::CubeLut& cube)
    {
        auto& tab = cube.GetTable();
        ID3D11ShaderResourceView* outSrv = nullptr;
        const auto* data = reinterpret_cast<const float*>(std::visit([](auto& x) { return x.GetRawData().data(); }, tab));

        D3D11_TEXTURE3D_DESC desc;
        ZeroMemory(&desc, sizeof(D3D11_TEXTURE3D_DESC));
        desc.Width = static_cast<UINT>(cube.Length());
        desc.Height = static_cast<UINT>(cube.Length());
        desc.Depth = static_cast<UINT>(cube.Length());
        desc.MipLevels = 1;
        desc.Format = DXGI_FORMAT_R32G32B32_FLOAT;
        desc.Usage = D3D11_USAGE_DEFAULT;
        desc.BindFlags = D3D11_BIND_SHADER_RESOURCE;
        desc.CPUAccessFlags = 0;

        Microsoft::WRL::ComPtr<ID3D11Texture3D> texture = nullptr;
        D3D11_SUBRESOURCE_DATA subResource;
        ZeroMemory(&subResource, sizeof(D3D11_SUBRESOURCE_DATA));
        subResource.pSysMem = data;
        subResource.SysMemPitch = desc.Width * sizeof(float) * 3;
        subResource.SysMemSlicePitch = static_cast<unsigned long long>(desc.Height) * desc.Width * sizeof(float) * 3;
        auto hr = dev->CreateTexture3D(&desc, &subResource, texture.GetAddressOf());
        if (FAILED(hr)) throw Ex(ToolException, "CreateTexture3D: {}", hr);

        D3D11_SHADER_RESOURCE_VIEW_DESC srvDesc;
        ZeroMemory(&srvDesc, sizeof(D3D11_SHADER_RESOURCE_VIEW_DESC));
        srvDesc.Format = DXGI_FORMAT_R32G32B32_FLOAT;
        srvDesc.ViewDimension = D3D11_SRV_DIMENSION_TEXTURE3D;
        srvDesc.Texture3D.MipLevels = desc.MipLevels;
        srvDesc.Texture3D.MostDetailedMip = 0;
        hr = dev->CreateShaderResourceView(texture.Get(), &srvDesc, &outSrv);
        if (FAILED(hr)) throw Ex(ToolException, "CreateShaderResourceView: {}", hr);

        return outSrv;
    }

}

#pragma region ToolData
#undef RGB
Enum(_NormalMapConvertFormat, RGB, DA)

namespace nlohmann
{
    template <>
    struct adl_serializer<U8String>
    {
        static void to_json(json& j, const U8String& v)
        {
            j = v.GetView().data();
        }

        static void from_json(const json& j, U8String& v)
        {
            std::string buf;
            j.get_to(buf);
            v.Set(reinterpret_cast<const char8_t*>(buf.data()));
        }
    };

    template <>
    struct adl_serializer<ImageTools::NormalMapConvert::Format>
    {
        static void to_json(json& j, const ImageTools::NormalMapConvert::Format& v)
        {
            j = EnumToString(static_cast<_NormalMapConvertFormat>(v));
        }

        static void from_json(const json& j, ImageTools::NormalMapConvert::Format& v)
        {
            v = static_cast<ImageTools::NormalMapConvert::Format>(EnumTo_NormalMapConvertFormat(j.get<std::string>()));
        }
    };

    template <>
    struct adl_serializer<ImVec4>
    {
        static void to_json(json& j, const ImVec4& v)
        {
            const auto& [x, y, z, w] = v;
            j = std::array{ x, y, z, w };
        }

        static void from_json(const json& j, ImVec4& v)
        {
            const auto& [x, y, z, w] = j.get<std::array<decltype(ImVec4::x), 4>>();
            v = ImVec4(x, y, z, w);
        }
    };

    template <>
    struct adl_serializer<Processor>
    {
        static void to_json(json& j, const Processor& v)
        {
            j = EnumToString(v);
        }

        static void from_json(const json& j, Processor& v)
        {
            v = EnumToProcessor(j.get<std::string>());
        }
    };
}
#pragma endregion ToolData

#pragma region Tools
template<typename Impl>
struct ITool
{
    constexpr static const char* Name()
    {
        return Impl::Name();
    }

    void UI(int64_t& needUpdate)
    {
        return static_cast<Impl*>(this)->UI();
    }

    [[nodiscard]] decltype(auto) Processor() const
    {
        return static_cast<Impl*>(this)->Processor();
    }

    [[nodiscard]] std::optional<ImageView> GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input)
    {
        return static_cast<Impl*>(this)->GPU(input);
    }
};

#define InitShader(func, sh)\
    static std::unordered_map<Dx11DevType*, Microsoft::WRL::ComPtr<ID3D11ComputeShader>> sh##Shaders = []()\
    {\
        try\
        {\
            return std::unordered_map<Dx11DevType*, Microsoft::WRL::ComPtr<ID3D11ComputeShader>>\
            {\
                { D3D11Dev.Get(), D3D11::CreateComputeShader(D3D11Dev.Get(), sh) },\
                { D3D11CSDev.Get(), D3D11::CreateComputeShader(D3D11CSDev.Get(), sh) }\
            };\
        }\
        catch (const std::exception& e)\
        {\
            std::rethrow_if_nested(Ex(ToolException, "[" #sh "] init shader failed"));\
        }\
        return std::unordered_map<Dx11DevType*, Microsoft::WRL::ComPtr<ID3D11ComputeShader>>{};\
    }();\
    const auto sh##Shader = sh##Shaders[dev].Get();

struct LutTool : ITool<LutTool>
{
    using ProcessorType = ImageTools::LUT;

    struct ToolData
    {
        U8String CubeFilePath{};
        
        NLOHMANN_DEFINE_TYPE_INTRUSIVE(ToolData, CubeFilePath)
    } Data;

    bool Valid = false;

    constexpr static const char* Name() { return U8"LUT"; }

    bool Check()
    {
        Valid = false;
        if (!Data.CubeFilePath.Empty() && exists(Data.CubeFilePath.GetPath())) Valid = true;
        return Valid;
    }

    void UI(int64_t& needUpdate)
    {
        ImGui::InputText(Text::CubeFile(), Data.CubeFilePath.GetAddress(), MaxPathLength8, ImGuiInputTextFlags_CallbackResize, U8String::ImGuiInputTextCallback, &Data.CubeFilePath);
        if (ImGui::IsItemEdited()) if (Check()) needUpdate = true;
        ImGui::SameLine();
        if (ImGui::Button(Text::SelectSomething()))
        {
	        if (const auto tmp = Pick::PickFile(L"Cube File\0*.cube\0").u8string(); !tmp.empty()) Data.CubeFilePath.Set(tmp);
            if (Check()) needUpdate = true;
        }
        if (!Valid) ImGui::TextColored({ 1.f, 0.f, 0.f, 1.f }, "* Invalid path");
    }

    [[nodiscard]] std::optional<ProcessorType> Processor() const
    {
        if (!Valid) return {};
        return ProcessorType(Data.CubeFilePath.GetView());
    }

    struct ShaderData
    {
        float MaxRGB[3];
        float MinRGB[3];
        float Size;
    };

    [[nodiscard]] std::optional<ImageView> GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input)
    {
        static Lut::CubeLut cube;

        if (!Valid) return {};

        if (static U8String path; path != Data.CubeFilePath)
        {
            path = Data.CubeFilePath;
            try
            {
                cube = Lut::CubeLut::FromCubeFile(Data.CubeFilePath.GetPath());
            }
            catch (...)
            {
                Valid = false;
                return {};
            }
        }

        InitShader("LutTool", g_LUT3D)

    	const auto tex3d = D3D11::CreateTexture3d(dev, cube);

        const auto resBuf = D3D11::CreateTexture2dUavBuf(dev, input.Width, input.Height);
        const auto resBufUav = D3D11::CreateTexture2dUav(dev, resBuf.Get());

        const auto ss = D3D11::CreateSampler(dev);

        auto [ar, ag, ab] = cube.DomainMax;
        auto [ir, ig, ib] = cube.DomainMin;

        float shaderData[7] = { ar, ag, ab, ir, ig, ib, static_cast<float>(cube.Length()) };
        const auto dataBuf = D3D11::CreateStructuredBuffer(dev, sizeof(ShaderData), 1, shaderData);
        const auto dataSrv = D3D11::CreateBufferSRV(dev, dataBuf.Get());

        ID3D11ShaderResourceView* srvs[] = { input.SRV.Get(), tex3d.Get(), dataSrv.Get() };
        ID3D11UnorderedAccessView* uavs[] = { resBufUav.Get() };
        ID3D11SamplerState* sss[] = { ss.Get() };
        
        D3D11::RunComputeShader(
            devCtx, g_LUT3DShader,
            srvs, uavs, sss,
            D3D11::GetThreadGroupNum(input.Width), D3D11::GetThreadGroupNum(input.Height), 1);

        tex3d->Release();

        return ImageView(input, D3D11::CreateSrvFromTex(dev, resBuf.Get()));
    }
};

struct LinearDodgeColorTool : ITool<LinearDodgeColorTool>
{
    using ProcessorType = ImageTools::LinearDodgeColor;

    struct ToolData
    {
        std::array<float, 4> Color = { 0, 0, 0, 1. };

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(ToolData, Color)
    } Data;

    constexpr static const char* Name() { return Text::LinearDodgeColor(); }

    void UI(int64_t& needUpdate)
    {
        ImGui::ColorEdit4(Text::Color(), Data.Color.data());
        if (ImGui::IsItemEdited()) needUpdate = true;
    }

    [[nodiscard]] std::optional<ProcessorType> Processor() const
    {
        return ProcessorType(Image::ColorRgba<uint8_t>(Data.Color[0] * 255., Data.Color[1] * 255., Data.Color[2] * 255., Data.Color[3] * 255.));
    }

    [[nodiscard]] std::optional<ImageView> GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input)
    {
        InitShader("LinearDodgeColorTool", g_LinearDodgeColor);

        const auto resBuf = D3D11::CreateTexture2dUavBuf(dev, input.Width, input.Height);
        const auto resBufUav = D3D11::CreateTexture2dUav(dev, resBuf.Get());

        const auto dataBuf = D3D11::CreateStructuredBuffer(dev, sizeof(float) * 4, 1, Data.Color.data());
        const auto dataSrv = D3D11::CreateBufferSRV(dev, dataBuf.Get());

        ID3D11ShaderResourceView* srvs[2] = { input.SRV.Get(), dataSrv.Get() };
        ID3D11UnorderedAccessView* uavs[] = { resBufUav.Get() };
        D3D11::RunComputeShader(
            devCtx, g_LinearDodgeColorShader,
            srvs, uavs,
            D3D11::GetThreadGroupNum(input.Width), D3D11::GetThreadGroupNum(input.Height), 1);

        return ImageView(input, D3D11::CreateSrvFromTex(dev, resBuf.Get()));
    }
};

struct LinearDodgeImageTool: ITool<LinearDodgeImageTool>
{
    using ProcessorType = ImageTools::LinearDodgeImage;

    struct ToolData
    {
        U8String ImagePath;

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(ToolData, ImagePath)
    } Data;

    bool Valid = false;

    constexpr static const char* Name() { return Text::LinearDodgeImage(); }

    bool Check()
    {
        Valid = false;
        if (!Data.ImagePath.Empty() && exists(Data.ImagePath.GetPath())) Valid = true;
        return Valid;
    }

    void UI(int64_t& needUpdate)
    {
        ImGui::InputText(Text::ImageFile(), Data.ImagePath.GetAddress(), MaxPathLength8, ImGuiInputTextFlags_CallbackResize, U8String::ImGuiInputTextCallback, &Data.ImagePath);
        if (ImGui::IsItemEdited()) if (Check()) needUpdate = true;
        ImGui::SameLine();
        if (ImGui::Button(Text::SelectSomething()))
        {
	        if (const auto p = Pick::PickFile(); !p.empty()) Data.ImagePath.Set(p.u8string());
            if (Check()) needUpdate = true;
        }
        if (!Valid) ImGui::TextColored({ 1.f, 0.f, 0.f, 1.f }, "* Invalid path");
    }

    [[nodiscard]] std::optional<ProcessorType> Processor() const
    {
        if (!Valid) return {};
        return ProcessorType(Data.ImagePath.GetPath());
    }

    [[nodiscard]] std::optional<ImageView> GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input)
    {
        static std::filesystem::path refPath{};
        static Image::ImageFile ref{};

        if (!Valid) return {};

        if (Data.ImagePath.GetPath() != refPath)
        {
            ref = Image::ImageFile(Data.ImagePath.GetPath());
            if (ref.Empty())
            {
                Valid = false;
                return {};
            }
            refPath = Data.ImagePath.GetPath();
        }

        InitShader("LinearDodgeImageTool", g_LinearDodgeImage);

        const auto resBuf = D3D11::CreateTexture2dUavBuf(dev, input.Width, input.Height);
        const auto resBufUav = D3D11::CreateTexture2dUav(dev, resBuf.Get());

        const auto refSrv = D3D11::LoadTextureFromFile(dev, ref);

        ID3D11ShaderResourceView* srvs[2] = { input.SRV.Get(), refSrv.SRV.Get() };
        ID3D11UnorderedAccessView* uavs[] = { resBufUav.Get() };
        D3D11::RunComputeShader(
            devCtx, g_LinearDodgeImageShader,
            srvs, uavs,
            D3D11::GetThreadGroupNum(input.Width), D3D11::GetThreadGroupNum(input.Height), 1);

        return ImageView(input, D3D11::CreateSrvFromTex(dev, resBuf.Get()));
    }
};

struct GenerateNormalTextureTool: ITool<GenerateNormalTextureTool>
{
    using ProcessorType = ImageTools::GenerateNormalTexture;

    struct ToolData
    {
        float Bias = 50.;
        bool InvertR = false;
        bool InvertG = false;

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(ToolData, Bias, InvertR, InvertG)
    } Data;

    constexpr static const char* Name() { return Text::GenerateNormalTexture(); }

    void UI(int64_t& needUpdate)
    {
        if (ImGui::Button(Text::EditValue())) ImGui::SetKeyboardFocusHere();
        ImGui::SameLine();
        ImGui::SliderFloat(U8"bias", &Data.Bias, 0.0f, 100.0f, "%.3f");
        if (ImGui::IsItemEdited()) needUpdate = true;
        ImGui::Checkbox(U8"invert R", &Data.InvertR);
        if (ImGui::IsItemEdited()) needUpdate = true;
        ImGui::Checkbox(U8"invert G", &Data.InvertG);
        if (ImGui::IsItemEdited()) needUpdate = true;
    }

    [[nodiscard]] std::optional<ProcessorType> Processor() const
    {
        return ProcessorType(Data.Bias, Data.InvertR, Data.InvertG);
    }

    struct ShaderData
    {
        float Bias;
        uint32_t InvertR;
        uint32_t InvertG;
        float Width;
        float Height;
    };

    [[nodiscard]] std::optional<ImageView> GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input)
    {
        InitShader("GenerateNormalTextureTool", g_GenerateNormalTexture);

        const auto resBuf = D3D11::CreateTexture2dUavBuf(dev, input.Width, input.Height);
        const auto resBufUav = D3D11::CreateTexture2dUav(dev, resBuf.Get());

        ShaderData data{ Data.Bias, Data.InvertR, Data.InvertG, static_cast<float>(input.Width), static_cast<float>(input.Height) };
        const auto dataBuf = D3D11::CreateStructuredBuffer(dev, sizeof(ShaderData), 1, &data);
        const auto dataSrv = D3D11::CreateBufferSRV(dev, dataBuf.Get());

        const auto sam = D3D11::CreateSampler(dev);

        ID3D11ShaderResourceView* srvs[2] = { input.SRV.Get(), dataSrv.Get() };
        ID3D11UnorderedAccessView* uavs[1] = { resBufUav.Get() };
        ID3D11SamplerState* sss[1] = { sam.Get() };

        D3D11::RunComputeShader(
            devCtx, g_GenerateNormalTextureShader,
            srvs, uavs, sss,
            D3D11::GetThreadGroupNum(input.Width), D3D11::GetThreadGroupNum(input.Height), 1);

        return ImageView(input, D3D11::CreateSrvFromTex(dev, resBuf.Get()));
    }
};

struct NormalMapConvertorTool: ITool<NormalMapConvertorTool>
{
    using ProcessorType = ImageTools::NormalMapConvert;

    struct ToolData
    {
        ProcessorType::Format InputType = ProcessorType::Format::RGB;
        ProcessorType::Format OutputType = ProcessorType::Format::DA;

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(ToolData, InputType, OutputType)
    } Data;

    constexpr static const char* Name() { return Text::NormalMapFormatConvert(); }

    void UI(int64_t& needUpdate)
    {
        ImGui::Text(Text::InputFormat());
        ImGui::SameLine();
        ImGui::RadioButton(U8"RGB##in", reinterpret_cast<int*>(&Data.InputType), 0);
        if (ImGui::IsItemEdited()) needUpdate = true;
        ImGui::SameLine();
        ImGui::RadioButton(U8"DA##in", reinterpret_cast<int*>(&Data.InputType), 1);
        if (ImGui::IsItemEdited()) needUpdate = true;
        
        ImGui::Text(Text::OutputFormat());
        ImGui::SameLine();
        ImGui::RadioButton(U8"RGB##out", reinterpret_cast<int*>(&Data.OutputType), 0);
        if (ImGui::IsItemEdited()) needUpdate = true;
        ImGui::SameLine();
        ImGui::RadioButton(U8"DA##out", reinterpret_cast<int*>(&Data.OutputType), 1);
        if (ImGui::IsItemEdited()) needUpdate = true;
    }

    [[nodiscard]] std::optional<ProcessorType> Processor() const
    {
        if (Data.InputType == Data.OutputType) return {};
        return ProcessorType(Data.InputType, Data.OutputType);
    }

    [[nodiscard]] std::optional<ImageView> GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input)
    {
        if (Data.InputType == Data.OutputType) return {};

        InitShader("NormalMapConvertorTool", g_NormalMapConvertorRGB2DA);

        const auto resBuf = D3D11::CreateTexture2dUavBuf(dev, input.Width, input.Height);
        const auto resBufUav = D3D11::CreateTexture2dUav(dev, resBuf.Get());

        ID3D11ShaderResourceView* srvs[] = { input.SRV.Get() };
        ID3D11UnorderedAccessView* uavs[] = { resBufUav.Get() };

        if (Data.InputType == ProcessorType::Format::RGB && Data.OutputType == ProcessorType::Format::DA)
        {
            D3D11::RunComputeShader(
                devCtx, g_NormalMapConvertorRGB2DAShader,
                srvs, uavs,
                D3D11::GetThreadGroupNum(input.Width), D3D11::GetThreadGroupNum(input.Height), 1);
        }
        else
        {
            throw Ex(ToolException, "[DA->RGB] not impl");
        }

        return ImageView(input, D3D11::CreateSrvFromTex(dev, resBuf.Get()));
    }
};

#undef InitShader
#pragma endregion Tools

namespace Config
{
    constexpr int Version[] = {1, 0, 0, 0};
    static constexpr auto WindowTitle = L"ImgTools";
    static constexpr auto FontSize = 20.f;
};

class ImgTools
{
#pragma region ImgToolsType
    using IoPath = std::pair<std::filesystem::path, std::filesystem::path>;
    using ToolType = std::variant
        <
            LutTool,
            LinearDodgeColorTool,
            LinearDodgeImageTool,
            GenerateNormalTextureTool,
            NormalMapConvertorTool
        >;
    using ProcessorType = std::variant
        <
            LutTool::ProcessorType,
            LinearDodgeColorTool::ProcessorType,
            LinearDodgeImageTool::ProcessorType,
            GenerateNormalTextureTool::ProcessorType,
            NormalMapConvertorTool::ProcessorType
        >;
#pragma endregion ImgToolsType

#pragma region ImgToolsStruct
    struct SettingData
    {
        ImVec4 ClearColor = ImVec4(0.07f, 0.07f, 0.07f, 1.00f);
        bool VSync = true;
        Processor ExportProcessor = Processor::GPU;
        Processor PreviewProcessor = Processor::GPU;

        static std::string ToJson(const SettingData& data)
        {
            return nlohmann::json(data).dump(4);
        }

        NLOHMANN_DEFINE_TYPE_INTRUSIVE(SettingData, ClearColor, VSync, ExportProcessor, PreviewProcessor)
    };
#pragma endregion ImgToolsStruct

#pragma region ImgToolsStatus
    // window
    WNDCLASSEX wc{};
    HWND mainWnd{};

    // show status
    bool showDemoWindows = true;
    bool showTools = true;
    bool showRaw = false;
    bool showPreview = false;
    bool showSettings = false;
    bool showInfo = true;
    bool showLicense = false;
    bool showAbout = false;
    bool showDocument = false;

    // proc status
    ImageFormat imgFormat = ImageFormat::png;
    bool needJoin = false;
    std::priority_queue<IoPath, std::vector<IoPath>, std::greater<>> procFiles{};
    std::atomic_int64_t processedCount = 0;
    int64_t totalCount = 0;
    float procStatus = 0.f;
    float procTimePreUpdate = 0.f;
    U8String curFile{};

    // ui status
    U8String inputPath{};
    U8String outputPath{};
    std::vector<ToolType> toolList{};
    bool done = false;

    // preview
    ImageView rawTexture;
    ImageView previewTexture;
    std::filesystem::path previewPath{};
    int64_t needUpdate = false;

    // config
    SettingData settingData{};
    bool needSaveSetting = false;
    std::filesystem::path configPath{};
    std::u8string iniPath{};
    std::filesystem::path settingsPath{};
    bool wantToSaveSetting = false;
#pragma endregion ImgToolsStatus

#pragma region ImgToolsHelper
    static Image::ImageFile ProcessFile(const Image::ImageFile& img, const std::vector<ToolType>& tools)
    {
	    Image::ImageFile cur = img;

        std::vector<ProcessorType> processors{};
        for (const auto& tool : tools)
        {
            if (const auto val = std::visit([](const auto& x) -> std::optional<ProcessorType> { return x.Processor(); }, tool); val.has_value())
            {
                processors.push_back(*val);
                std::visit([&](auto& x) { x.ImgRef(cur); }, processors[processors.size() - 1]);
            }
        }

        for (auto& proc : processors)
        {
            const auto [w, h] = std::visit([](const auto& x) -> ImageTools::ImageSize { return x.GetOutputSize(); }, proc);
            auto buf = Image::ImageFile(w, h);

            Enumerable::Range<int64_t> rng(w * h);
            std::for_each(std::execution::par_unseq, rng.begin(), rng.end(), [&](const auto idx)
                {
                    const int row = idx / w;
                    const int col = idx - row * w;
                    buf.Set(row, col, std::visit([&](auto& x) { return x(row, col); }, proc));
                });

            cur = buf;
        }

        return cur;
    }

    static ImageView GPU(Dx11DevType* dev, Dx11DevCtxType* devCtx, const ImageView& input, std::vector<ToolType>& tools)
    {
        ImageView imgPrev = input;
        for (auto& tool : tools)
        {
	        if (auto out = std::visit([&](auto& t) { return t.GPU(dev, devCtx, imgPrev); }, tool); out.has_value()) imgPrev = std::move(*out);
        }
        devCtx->Flush();
        return imgPrev;
    }

    static Image::ImageFile ProcessFileGpu(Dx11DevType* dev, Dx11DevCtxType* devCtx, const Image::ImageFile& input, std::vector<ToolType>& tools)
    {
        return D3D11::CreateOutTexture(dev, devCtx, GPU(dev, devCtx, D3D11::LoadTextureFromFile(D3D11CSDev.Get(), Image::ImageFile(input)), tools));
    }
#pragma endregion ImgToolsHelper

    static LRESULT WINAPI WndProc(HWND hWnd, UINT msg, WPARAM wParam, LPARAM lParam)
    {
        if (ImGui_ImplWin32_WndProcHandler(hWnd, msg, wParam, lParam)) return true;

        switch (msg)
        {
        case WM_SIZE:
            if (D3D11Dev != nullptr && wParam != SIZE_MINIMIZED)
            {
                D3D11::CleanupRenderTarget();
                D3D11SwapChain->ResizeBuffers(0, LOWORD(lParam), HIWORD(lParam), DXGI_FORMAT_UNKNOWN, 0);
                D3D11::CreateRenderTarget();
            }
            return 0;
        case WM_SYSCOMMAND:
            if ((wParam & 0xfff0) == SC_KEYMENU) return 0;
            break;
        case WM_DESTROY:
            PostQuitMessage(0);
            if (IsProcessing) ProcThread.join();
            return 0;
        }
        return DefWindowProc(hWnd, msg, wParam, lParam);
    }

#pragma region Init
    void Init()
    {
        try
        {
            InitWindow();
            InitImgui();
            InitData();
        }
        catch (...)
        {
            std::throw_with_nested(Ex(Exception, "init failed"));
        }
    }

    void InitWindow()
    {
        ImGui_ImplWin32_EnableDpiAwareness();
    	wc =
        {
            sizeof(WNDCLASSEX),
            CS_CLASSDC,
            WndProc,
            0L,
            0L,
            GetModuleHandle(nullptr),
            nullptr,
            nullptr,
            nullptr,
            nullptr,
            Config::WindowTitle,
            nullptr
        };
        RegisterClassEx(&wc);

        mainWnd = CreateWindow
        (
            wc.lpszClassName,
            Config::WindowTitle,
            WS_OVERLAPPEDWINDOW,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            CW_USEDEFAULT,
            nullptr,
            nullptr,
            wc.hInstance,
            nullptr
        );

        InitDirect3D11();

        ShowWindow(mainWnd, SW_SHOWDEFAULT);
        UpdateWindow(mainWnd);
    }

    void InitDirect3D11() const
    {
        try
        {
            D3D11::CreateDeviceD3D(mainWnd);
        }
        catch (...)
        {
            D3D11::CleanupDeviceD3D();
            UnregisterClass(wc.lpszClassName, wc.hInstance);
            std::throw_with_nested(Ex(ImgToolsException, "init dx11 error"));
        }
    }

    void InitImgui()
    {
        IMGUI_CHECKVERSION();
        ImGui::CreateContext();

        configPath = GetAppData() / "ImgTools";
        if (!exists(configPath)) std::filesystem::create_directory(configPath);

        iniPath = (configPath / "config.ini").u8string();
        if (const auto fp = std::filesystem::path(iniPath); !exists(fp)) std::ofstream(fp).close();

        InitImguiImplIniSettings();

        ImGui::StyleColorsDark();

        ImGui_ImplWin32_Init(mainWnd);
        ImGui_ImplDX11_Init(D3D11Dev.Get(), D3D11DevCtx.Get());

        std::string font{};
        for (const auto& seg : RHR::ScRegular) font.append(seg);

        const ImGuiIO& io = ImGui::GetIO();
        io.Fonts->AddFontFromMemoryCompressedBase85TTF(font.c_str(), Config::FontSize, nullptr, io.Fonts->GetGlyphRangesChineseFull());
    }

    void InitImguiImplIniSettings() const
    {
        ImGuiIO& io = ImGui::GetIO();
        io.IniFilename = nullptr;
        ImGui::LoadIniSettingsFromDisk(reinterpret_cast<const char*>(iniPath.c_str()));
    }

    void InitData()
    {
        settingsPath = (configPath / "settings.json");
        if (!exists(settingsPath))
        {
            File::WriteAll(settingsPath, SettingData::ToJson(settingData));
        }
        else
        {
            settingData = nlohmann::json::parse(File::ReadAll(settingsPath));
        }
    }
#pragma endregion Init

#pragma region Update
    void Update()
    {
        UpdatePreview();
        UpdateImgui();
        UpdateUi();
        UpdateRender();
        UpdateSetting();
    }

    void UpdatePreview()
    {
        if (!needUpdate) return;

        const auto t1 = std::chrono::high_resolution_clock::now();
        
        if (rawTexture.SRV)
        {
            if (settingData.PreviewProcessor == Processor::GPU)
            {
                previewTexture = GPU(D3D11Dev.Get(), D3D11DevCtx.Get(), rawTexture, toolList);
                D3D11DevCtx->Flush();
            }
            else
            {
                previewTexture = D3D11::LoadTextureFromFile(D3D11Dev.Get(), ProcessFile(Image::ImageFile(previewPath), toolList));
            }
        }

        const auto t2 = std::chrono::high_resolution_clock::now();
        procTimePreUpdate = std::chrono::duration_cast<std::chrono::duration<float, std::micro>>(t2 - t1).count();

        needUpdate = false;
    }

    static void UpdateImgui()
    {
        ImGui_ImplDX11_NewFrame();
        ImGui_ImplWin32_NewFrame();
        ImGui::NewFrame();
    }

    void UpdateRender() const
    {
        ImGui::Render();

        const auto col = settingData.ClearColor;
        const float clearColorWithAlpha[4] = { col.x * col.w, col.y * col.w, col.z * col.w, col.w };
        D3D11DevCtx->OMSetRenderTargets(1, D3D11MainRenderTargetView.GetAddressOf(), nullptr);
        D3D11DevCtx->ClearRenderTargetView(D3D11MainRenderTargetView.Get(), clearColorWithAlpha);
        ImGui_ImplDX11_RenderDrawData(ImGui::GetDrawData());

        D3D11SwapChain->Present(settingData.VSync, 0);
    }

    void UpdateUi()
    {
#if defined(DEBUG) | defined(_DEBUG)
        if (showDemoWindows) ImGui::ShowDemoWindow(&showDemoWindows);
#endif
        ShowInfo();
        ShowSetting();
        ShowRaw();
        ShowPreview();
        ShowImageTools();
        ShowTopMenu();
    }

    void UpdateSetting() const
    {
        if (ImGui::GetIO().WantSaveIniSettings) ImGui::SaveIniSettingsToDisk(reinterpret_cast<const char*>(iniPath.c_str()));
        if (wantToSaveSetting) File::WriteAll(settingsPath, SettingData::ToJson(settingData));
    }

    void ShowInfo()
    {
        if (showInfo)
        {
            constexpr ImGuiWindowFlags windowFlags =
                ImGuiWindowFlags_NoDecoration |
                ImGuiWindowFlags_AlwaysAutoResize |
                ImGuiWindowFlags_NoSavedSettings |
                ImGuiWindowFlags_NoFocusOnAppearing |
                ImGuiWindowFlags_NoNav |
                ImGuiWindowFlags_NoMove;

            constexpr auto pad = 10.0f;
            ImVec2 pos = ImGui::GetMainViewport()->WorkPos;
            pos.x = pos.x + pad;
            pos.y = pos.y + pad;
            ImGui::SetNextWindowPos(pos, ImGuiCond_Always);

            ImGui::SetNextWindowBgAlpha(0.35f);
            if (ImGui::Begin("Info", &showInfo, windowFlags))
            {
                ImGui::Text("FrameRate: %.1f FPS", ImGui::GetIO().Framerate);
                ImGui::Text("Update: %.1f ms", procTimePreUpdate / 1000.f);
            }

            ImGui::End();
        }
    }

    void ShowSetting()
    {
        if (showSettings)
        {
            ImGui::Begin(Text::Setting(), &showSettings);

            ImGui::ColorEdit3(Text::BackgroundColor(), reinterpret_cast<float*>(&settingData.ClearColor));
            if (ImGui::IsItemEdited()) wantToSaveSetting = true;
            
            ImGui::Separator();
            ImGui::Text(Text::ProcessorPreview());
            ImGui::SameLine();
            ImGui::RadioButton(U8"CPU##ProcessorPreview", reinterpret_cast<int*>(&settingData.PreviewProcessor), static_cast<int>(Processor::CPU));
            if (ImGui::IsItemEdited()) { needUpdate = true; wantToSaveSetting = true; }
            ImGui::SameLine();
            ImGui::RadioButton(U8"GPU##ProcessorPreview", reinterpret_cast<int*>(&settingData.PreviewProcessor), static_cast<int>(Processor::GPU));
            if (ImGui::IsItemEdited()) { needUpdate = true; wantToSaveSetting = true; }

            ImGui::Separator();
            ImGui::Text(Text::ProcessorExport());
            ImGui::SameLine();
            ImGui::RadioButton(U8"CPU##ProcessorExport", reinterpret_cast<int*>(&settingData.ExportProcessor), static_cast<int>(Processor::CPU));
            if (ImGui::IsItemEdited()) wantToSaveSetting = true;
            ImGui::SameLine();
            ImGui::RadioButton(U8"GPU##ProcessorExport", reinterpret_cast<int*>(&settingData.ExportProcessor), static_cast<int>(Processor::GPU));
            if (ImGui::IsItemEdited()) wantToSaveSetting = true;

            if (ImGui::Button("Reset Settings"))
            {
                settingData = {};
                needUpdate = true;
                wantToSaveSetting = true;
            }

            ImGui::End();
        }
    }

    void ShowRaw()
    {
        if (showRaw)
        {
            ImGui::Begin(Text::RawImage(), &showRaw);
            const auto imgW = rawTexture.Width;
            const auto imgH = rawTexture.Height;
            ImGui::Text("%d x %d", imgW, imgH);
            if (rawTexture.Height && previewTexture.Height)
            {
	            const auto size = ImGui::GetContentRegionAvail();
                auto h = size.y;
                auto w = h * imgW / imgH;
                if (w > size.x)
                {
                    w = size.x;
                    h = w * imgH / imgW;
                }
                if (w == 0 || h == 0) ImGui::SetWindowSize(ImVec2(400, 400));
                ImGui::Image(rawTexture.SRV.Get(), ImVec2(w, h));
            }
            ImGui::End();
        }
    }

    void ShowPreview()
    {
        if (showPreview)
        {
            ImGui::Begin(Text::Preview(), &showPreview);
            const auto imgW = previewTexture.Width;
            const auto imgH = previewTexture.Height;
            ImGui::Text("%d x %d", imgW, imgH);
            if (previewTexture.Height && previewTexture.Width)
            {
	            const auto size = ImGui::GetContentRegionAvail();
                auto h = size.y;
                auto w = h * imgW / imgH;
                if (w > size.x)
                {
                    w = size.x;
                    h = w * imgH / imgW;
                }
                if (w == 0 || h == 0) ImGui::SetWindowSize(ImVec2(400, 400));
                ImGui::Image(previewTexture.SRV.Get(), ImVec2(w, h));
            }
            ImGui::End();
        }
    }

#pragma region UpdateImageTools
    void ShowImageTools()
    {
        if (showTools)
        {
            ImGui::Begin("image tools", &showTools);

            ShowImageToolsImplInput();
            ShowImageToolsImplOutput();

            ImGui::Separator();
            if (ImGui::BeginTabBar("Processor", ImGuiTabBarFlags_AutoSelectNewTabs /*| ImGuiTabBarFlags_Reorderable*/ | ImGuiTabBarFlags_FittingPolicyResizeDown))
            {
                ShowImageToolsImplExport();
                ShowImageToolsImplAdd();
                ShowImageToolsImplTab();

                ImGui::EndTabBar();
            }

            ImGui::End();
        }
    }

    void ShowImageToolsImplInput()
    {
        ImGui::InputText(Text::InputPath(), inputPath.GetAddress(), MaxPathLength8, ImGuiInputTextFlags_CallbackResize, U8String::ImGuiInputTextCallback, &inputPath);
        ImGui::SameLine();
        if (ImGui::Button(reinterpret_cast<const char*>(String::FormatU8("{}##input" __FUNCTION__, NormU8(Text::SelectSomething())).c_str())))
        {
            const auto buf = Pick::PickFileAndFolder();
            
            if (buf.empty()) return;

            if (!exists(buf)) return;
 
            previewPath.clear();
            rawTexture = {};
            inputPath = String::ToU8String(buf);
            if (is_regular_file(buf))
            {
                outputPath = String::ToU8String(buf.parent_path() / String::FormatW("{}.out{}", buf.stem(), buf.extension()));
                previewPath = inputPath.GetView();
            }

            if (is_directory(buf))
            {
                outputPath = String::FormatU8("{}.out", buf);

                if (auto files = GetFiles(inputPath.GetView()); !files.empty())
                {
                    std::ranges::sort(files);
                    if (auto ff = files | std::views::filter([](const std::filesystem::path& file)
                        {
                            static std::unordered_set<std::string> extensions
                            {
                                ".jpg", ".jpeg", ".jpe", ".png", ".tga", ".bmp", ".psd", ".gif", ".hdr", ".pic", ".ppm", ".pgm"
                            };
                            return extensions.contains(String::ToLower(file.extension().string()));
                        }); std::ranges::distance(ff) != 0) previewPath = *ff.begin();
                }
            }

            if (!previewPath.empty())
            {
	            if (const auto newImg = Image::ImageFile(previewPath); !newImg.Empty())
                    rawTexture = D3D11::LoadTextureFromFile(D3D11Dev.Get(), newImg);
            }

            showRaw = true;
            showPreview = true;
            needUpdate = true;
        }
    }

    void ShowImageToolsImplOutput()
    {
        ImGui::InputText(Text::OutputPath(), outputPath.GetAddress(), MaxPathLength8, ImGuiInputTextFlags_CallbackResize, U8String::ImGuiInputTextCallback, &outputPath);
        ImGui::SameLine();
        if (ImGui::Button(reinterpret_cast<const char*>(String::FormatU8("{}##output", NormU8(Text::SelectSomething())).c_str())))
        {
            const auto buf = Pick::PickFileAndFolder();
            const auto input = inputPath.GetPath();
            if (exists(buf))
            {
                if (is_regular_file(input))
                {
                    if (is_regular_file(buf)) outputPath = buf.u8string();
                    if (is_directory(buf)) outputPath = String::ToU8String(buf / String::FormatW("{}.out{}", input.stem(), input.extension()));
                }

                if (is_directory(input))
                {
                    if (is_directory(buf)) outputPath = buf.u8string();
                    if (is_regular_file(buf)) outputPath = buf.parent_path().u8string();
                }
            }
            else
            {
                outputPath = buf.u8string();
            }
        }
    }

    void ShowImageToolsImplExport()
    {
        if (ImGui::BeginTabItem(Text::Export(), nullptr, ImGuiTabItemFlags_Trailing | ImGuiTabItemFlags_NoCloseWithMiddleMouseButton))
        {
            ImGui::Text(curFile.GetAddress());
            if (totalCount) procStatus = static_cast<float>(processedCount.load()) / totalCount;
            ImGui::ProgressBar(procStatus);

            ImGui::BeginDisabled(IsProcessing);
            constexpr const char* items[4] {"jpg", "png", "bmp", "tga"};
            ImGui::Combo(Text::Format(), reinterpret_cast<int*>(&imgFormat), items, 4);
            ImGui::EndDisabled();

            if (!IsProcessing && ImGui::Button(Text::Start()))
            {
                if (IsFilePath(inputPath))
                {
                    const auto path = inputPath.GetPath();
                    procFiles.emplace(path, path.parent_path() / String::FormatW("{}.{}", path.stem(), EnumToString(imgFormat)));
                }
                else if (IsFolderPath(inputPath))
                {
                    for (const auto& it : std::filesystem::directory_iterator(inputPath.GetView()))
                    {
                        const auto& path = it.path();
                        if (it.is_regular_file()) procFiles.emplace(path, outputPath.GetPath() / String::FormatW("{}.{}", path.stem(), EnumToString(imgFormat)));
                    }
                }
                IsProcessing = true;
                processedCount.store(0);
                totalCount = procFiles.size();
                needJoin = false;

                ProcThread = std::jthread([&](const std::stop_token& tk)
                    {
                        while (!procFiles.empty())
                        {
                            if (tk.stop_requested())
                            {
                                procFiles = {};
                                break;
                            }

                            const auto& [in, out] = procFiles.top();
                            LogInfo(R"("{}" => "{}")", in, out);

                            try
                            {
                                curFile = in.u8string();
                                if (!exists(out.parent_path())) std::filesystem::create_directory(out.parent_path());
                                if (settingData.ExportProcessor == Processor::GPU) ProcessFileGpu(D3D11CSDev.Get(), D3D11CSDevCtx.Get(), Image::ImageFile(in), toolList).Save(out);
                                else ProcessFile(Image::ImageFile(in), toolList).Save(out);
                            }
                            catch (const std::exception& ex)
                            {
                                LogErr("[ProcThread] processor error:\n{}", LogMsg::LogException(ex));
                            }

                            ++processedCount;
                            procFiles.pop();
                        }
                        curFile.Set(NormU8(Text::Finished()));
                        totalCount = 0;
                        procStatus = 0.f;
                        IsProcessing = false;
                        needJoin = true;
                    });
            }

            if (IsProcessing && ImGui::Button(Text::Cancel())) ProcThread.request_stop();

            if (needJoin)
            {
                ProcThread.join();
                needJoin = false;
            }

            ImGui::EndTabItem();
        }
    }

    void ShowImageToolsImplAdd()
    {
        if (ImGui::TabItemButton("+", ImGuiTabItemFlags_Trailing | ImGuiTabItemFlags_NoTooltip))
        {
            ImGui::OpenPopup("AddMenu");
        }

        if (ImGui::BeginPopup("AddMenu"))
        {
#define AutoSelect(tool) if (ImGui::Selectable(tool::Name())) { toolList.push_back(tool{}); needUpdate = true; }
            AutoSelect(LutTool)
            AutoSelect(LinearDodgeColorTool)
            AutoSelect(LinearDodgeImageTool)
            AutoSelect(GenerateNormalTextureTool)
            AutoSelect(NormalMapConvertorTool)
#undef AutoSelect

            ImGui::EndPopup();
        }
    }

    void ShowImageToolsImplTab()
    {
        for (std::size_t i = 0; i < toolList.size();)
        {
            bool open = true;
            std::visit([&](auto& x)
                {
                    if (ImGui::BeginTabItem(std::format("{}. {}", i + 1, x.Name()).c_str(), &open, ImGuiTabItemFlags_None))
                    {
                        const auto ps = IsProcessing;
                        if (ps) ImGui::BeginDisabled();
                        try
                        {
                            x.UI(needUpdate);
                        }
                        catch (...)
                        {
                            std::rethrow_if_nested(Ex(ImgToolsException, R"(render tool[index = i, name = "{}"] error)", x.Name()));
                        }
                        if (ps) ImGui::EndDisabled();
                        ImGui::EndTabItem();
                    }
                }, toolList[i]);

            if (!open)
            {
                toolList.erase(toolList.begin() + i);
                needUpdate = true;
            }
            else
            {
                i++;
            }
        }
    }
#pragma endregion UpdateImageTools

#pragma region UpdateTopMenu
    void ShowTopMenu()
    {
        if (ImGui::BeginMainMenuBar())
        {
            ShowTopMenuImplFile();
            ShowTopMenuImplWindow();
            ShowTopMenuImplHelp();
            
            ImGui::EndMainMenuBar();
        }
    }

    void ShowTopMenuImplFile()
    {
        if (ImGui::BeginMenu(Text::File()))
        {
            //if (ImGui::MenuItem("New", "CTRL+N")) {}
            //ImGui::Separator();
            if (ImGui::MenuItem("Settings", "CTRL+,")) showSettings = !showSettings;
            
            ImGui::Separator();
            if (ImGui::MenuItem("Exit", "ALT+F4")) done = true;

            ImGui::EndMenu();
        }
    }

    void ShowTopMenuImplWindow()
    {
        if (ImGui::BeginMenu(Text::Window()))
        {
            if (ImGui::MenuItem("Raw")) showRaw = !showRaw;
            if (ImGui::MenuItem("Preview")) showPreview = !showPreview;

            ImGui::EndMenu();
        }
    }

    void ShowTopMenuImplHelp()
    {
        if (ImGui::BeginMenu(Text::About()))
        {
            if (ImGui::MenuItem("Document")) showDocument = !showDocument;
            if (ImGui::MenuItem("License")) showLicense = !showLicense;
            if (ImGui::MenuItem("About")) showAbout = !showAbout;

            ImGui::EndMenu();
        }
        ShowTopMenuImplHelpImplDocument();
        ShowTopMenuImplHelpImplLicense();
        ShowTopMenuImplHelpImplAbout();
    }

    void ShowTopMenuImplHelpImplLicense()
    {
        if (!showLicense) return;

        ImGui::Begin("License", &showLicense);

        ImGui::Text(R"(-------------------------------------------------------------------------------)");
        ImGui::Text(R"(                                   ImgTools)");
        ImGui::Text(R"(-------------------------------------------------------------------------------)");
        ImGui::Text(R"(MIT License)");
        ImGui::Text(R"()");
        ImGui::Text(R"(Copyright (c) 2022 iriszero)");
        ImGui::Text(R"()");
        ImGui::Text(R"(Permission is hereby granted, free of charge, to any person obtaining a copy)");
        ImGui::Text(R"(of this software and associated documentation files (the "Software"), to deal)");
        ImGui::Text(R"(in the Software without restriction, including without limitation the rights)");
        ImGui::Text(R"(to use, copy, modify, merge, publish, distribute, sublicense, and/or sell)");
        ImGui::Text(R"(copies of the Software, and to permit persons to whom the Software is)");
        ImGui::Text(R"(furnished to do so, subject to the following conditions:)");
        ImGui::Text(R"()");
        ImGui::Text(R"(The above copyright notice and this permission notice shall be included in all)");
        ImGui::Text(R"(copies or substantial portions of the Software.)");
        ImGui::Text(R"()");
        ImGui::Text(R"(THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR)");
        ImGui::Text(R"(IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,)");
        ImGui::Text(R"(FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE)");
        ImGui::Text(R"(AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER)");
        ImGui::Text(R"(LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,)");
        ImGui::Text(R"(OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE)");
        ImGui::Text(R"(SOFTWARE.)");
        ImGui::Text(R"()");

        ImGui::Text(R"(-------------------------------------------------------------------------------)");
        ImGui::Text(R"(                                 ocornut/imgui)");
        ImGui::Text(R"(-------------------------------------------------------------------------------)");
        ImGui::Text(R"(The MIT License (MIT))");
        ImGui::Text(R"()");
        ImGui::Text(R"(Copyright (c) 2014-2022 Omar Cornut)");
        ImGui::Text(R"()");
        ImGui::Text(R"(Permission is hereby granted, free of charge, to any person obtaining a copy)");
        ImGui::Text(R"(of this software and associated documentation files (the "Software"), to deal)");
        ImGui::Text(R"(in the Software without restriction, including without limitation the rights)");
        ImGui::Text(R"(to use, copy, modify, merge, publish, distribute, sublicense, and/or sell)");
        ImGui::Text(R"(copies of the Software, and to permit persons to whom the Software is)");
        ImGui::Text(R"(furnished to do so, subject to the following conditions:)");
        ImGui::Text(R"()");
        ImGui::Text(R"(The above copyright notice and this permission notice shall be included in all)");
        ImGui::Text(R"(copies or substantial portions of the Software.)");
        ImGui::Text(R"()");
        ImGui::Text(R"(THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR)");
        ImGui::Text(R"(IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,)");
        ImGui::Text(R"(FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE)");
        ImGui::Text(R"(AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER)");
        ImGui::Text(R"(LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,)");
        ImGui::Text(R"(OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE)");
        ImGui::Text(R"(SOFTWARE.)");
        ImGui::Text(R"()");

        ImGui::Text(R"(-------------------------------------------------------------------------------)");
        ImGui::Text(R"(                                 nlohmann/json)");
        ImGui::Text(R"(-------------------------------------------------------------------------------)");
        ImGui::Text(R"(MIT License )");
        ImGui::Text(R"()");
        ImGui::Text(R"(Copyright (c) 2013-2022 Niels Lohmann)");
        ImGui::Text(R"()");
        ImGui::Text(R"(Permission is hereby granted, free of charge, to any person obtaining a copy)");
        ImGui::Text(R"(of this software and associated documentation files (the "Software"), to deal)");
        ImGui::Text(R"(in the Software without restriction, including without limitation the rights)");
        ImGui::Text(R"(to use, copy, modify, merge, publish, distribute, sublicense, and/or sell)");
        ImGui::Text(R"(copies of the Software, and to permit persons to whom the Software is)");
        ImGui::Text(R"(furnished to do so, subject to the following conditions:)");
        ImGui::Text(R"()");
        ImGui::Text(R"(The above copyright notice and this permission notice shall be included in all)");
        ImGui::Text(R"(copies or substantial portions of the Software.)");
        ImGui::Text(R"()");
        ImGui::Text(R"(THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR)");
        ImGui::Text(R"(IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,)");
        ImGui::Text(R"(FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE)");
        ImGui::Text(R"(AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER)");
        ImGui::Text(R"(LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,)");
        ImGui::Text(R"(OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE)");
        ImGui::Text(R"(SOFTWARE.)");

        ImGui::End();
    }

    void ShowTopMenuImplHelpImplAbout()
    {
        if (!showAbout) return;

        ImGui::Begin("About", &showAbout);

        ImGui::Text(R"(ImgTools)");
        ImGui::Text(R"()");
        ImGui::Text(R"(Copyright (c) 2022 iriszero(ih@iriszero.cc))");
        ImGui::Text(R"(Version %d.%d.%d.%d, Build %s %s)",
            Config::Version[0], Config::Version[1], Config::Version[2], Config::Version[3],
            __DATE__, __TIME__);

        ImGui::End();
    }

    void ShowTopMenuImplHelpImplDocument()
    {
        if (!showDocument) return;

        ImGui::Begin("Document", &showDocument);
        ImGui::Text(R"(
Helper

no help!)");
        ImGui::End();
    }
#pragma endregion UpdateTopMenu

#pragma endregion Update

public:
    ImgTools() { Init(); }

    void Run()
    {
        while (!done)
        {
            MSG msg;
            while (PeekMessage(&msg, nullptr, 0U, 0U, PM_REMOVE))
            {
                TranslateMessage(&msg);
                DispatchMessage(&msg);
                if (msg.message == WM_QUIT) done = true;
            }
            if (done) break;

            Update();
        }
    }

    ~ImgTools()
    {
        ImGui_ImplDX11_Shutdown();
        ImGui_ImplWin32_Shutdown();
        ImGui::DestroyContext();

        D3D11::CleanupDeviceD3D();
        DestroyWindow(mainWnd);
        UnregisterClass(wc.lpszClassName, wc.hInstance);
    }
};

class SingleInstance
{
    HANDLE mutex{};

public:
    bool Ok()
    {
        mutex = CreateMutex(nullptr, true, LR"(=Zz,EKn@O8-GJ(lO$l^6IXWGMGrzU]3QaJ-Itcx2ODg.=0~!FcItcx2ODg.=2v=IF)");
        return GetLastError() != ERROR_ALREADY_EXISTS;
    }

    ~SingleInstance() { if (mutex) CloseHandle(mutex); }
};

static SingleInstance AppInstance{};

int main(int argc, char** argv)
{
    if (!AppInstance.Ok())
    {
        MessageBox(nullptr, L"Already running.", L"Error", MB_ICONSTOP | MB_TASKMODAL);
        exit(EXIT_FAILURE);
    }

    system("chcp 65001");

#if defined(DEBUG) | defined(_DEBUG)
    _CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif

    std::thread logThread([]()
        {
            std::unordered_map<LogLevel, Console::Color> colorMap
            {
                { LogLevel::None,  Console::Color::White },
                { LogLevel::Error, Console::Color::Red },
                { LogLevel::Warn,  Console::Color::Yellow },
                { LogLevel::Log,   Console::Color::White },
                { LogLevel::Info,  Console::Color::Gray },
                { LogLevel::Debug, Console::Color::Blue }
            };

            while (true)
            {
                const auto [level, raw] = Log.Chan.Read();
                const auto& [time, id, file, line, func, msg] = raw;

                auto out = String::FormatU8("[{}] [{}] [{}] [{}:{}] [{}] {}",
                    EnumToString(level),
                    LogMsg::LogTime(time),
                    String::FromStream(id, std::hex),
                    file, line,
                    func,
                    msg);
                if (out[out.length() - 1] == L'\n') out.erase(out.length() - 1);

                Console::SetForegroundColor(colorMap.at(level));
                Console::WriteLine((const char*)out.c_str());
                Console::WriteLine();

                if (level == LogLevel::None)
                {
                    break;
                }
            }
        });

    LogLog(NormU8(Text::NowLoading()));
#if !(defined(DEBUG) || defined(_DEBUG))
    try
#endif
    {
        ImgTools{}.Run();
        LogNone("See you next time.");
    }
#if !(defined(DEBUG) || defined(_DEBUG))
    catch (const std::exception& e)
    {
        LogErr("Run error:\n{}", LogMsg::LogException(e));
        LogNone("Opss.");
        system("pause");
    }
#endif
    logThread.join();
}
