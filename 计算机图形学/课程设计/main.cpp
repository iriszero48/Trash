#include "../../Common/d3dApp.h" // 基本d3d结构
#include "../../Common/MathHelper.h" // 基本数学包装
#include "../../Common/UploadBuffer.h" // 上传缓冲区包装
#include "../../Common/GeometryGenerator.h" // 生成模型包装
#include "FrameResource.h" // 帧资源包装
#include "Waves.h" // 生成波纹

#pragma comment(lib, "d3dcompiler.lib")
#pragma comment(lib, "D3D12.lib")

//帧资源数
const int NumFrameResources = 3;

//渲染项
struct RenderItem
{
	RenderItem() = default;

	// 世界坐标
	DirectX::XMFLOAT4X4 World = MathHelper::Identity4x4();

	// 指示对象数据已更改
	int NumFramesDirty = NumFrameResources;

	// 常量缓冲区索引
	UINT ObjCBIndex = -1;

	// 材质
	Material* Mat = nullptr;

	// 模型
	MeshGeometry* Geo = nullptr;

	// 拓扑结构
	D3D12_PRIMITIVE_TOPOLOGY PrimitiveType = D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST;

	// 绘制索引总数
	UINT IndexCount = 0;

	// 绘制索引开始
	UINT StartIndexLocation = 0;

	// 绘制索引偏移
	int BaseVertexLocation = 0;
};

//渲染层
enum class RenderLayer : int
{
	Opaque = 0,
	Count
};

//主类
class LitWavesApp final : public D3DApp
{
public:
	LitWavesApp() = delete;
	explicit LitWavesApp(HINSTANCE hInstance);

	LitWavesApp(const LitWavesApp& rhs) = delete;
	LitWavesApp(LitWavesApp&& rhs) = delete;

	LitWavesApp& operator=(const LitWavesApp& rhs) = delete;
	LitWavesApp& operator=(LitWavesApp&& rhs) = delete;

	~LitWavesApp();

	// 初始化
	bool Initialize() override;

private:
	// 窗口缩放
	void OnResize() override;
	
	// 更新
	void Update(const GameTimer& gt) override;
	
	// 绘制
	void Draw(const GameTimer& gt) override;

	// 处理鼠标按下
	void OnMouseDown(WPARAM btnState, int x, int y) override;

	// 处理鼠标抬起
	void OnMouseUp(WPARAM btnState, int x, int y) override;

	// 处理鼠标移动
	void OnMouseMove(WPARAM btnState, int x, int y) override;

	// 处理键盘输入
	void OnKeyboardInput(const GameTimer& gt);

	// 更新摄像机
	void UpdateCamera(const GameTimer& gt);

	// 更新物体常量缓冲区
	void UpdateObjectCBs(const GameTimer& gt);

	// 更新材质常量缓冲区
	void UpdateMaterialCBs(const GameTimer& gt);

	// 更新Pass常量缓冲区
	void UpdateMainPassCB(const GameTimer& gt);

	// 更新波浪
	void UpdateWaves(const GameTimer& gt) const;

	// 创建根签名
	void BuildRootSignature();

	// 创建着色器和输入层
	void BuildShadersAndInputLayout();

	// 创建陆地模型
	void BuildLandGeometry();

	// 创建波浪模型缓冲区
	void BuildWavesGeometryBuffers();

	// 创建流水线状态对象
	void BuildPSOs();

	// 创建帧资源
	void BuildFrameResources();

	// 创建材质
	void BuildMaterials();

	// 创建渲染项
	void BuildRenderItems();

	// 绘制所有渲染项
	void DrawRenderItems(ID3D12GraphicsCommandList* cmdList, const std::vector<RenderItem*>& ritems) const;

	// 获取地形高度
	static float GetHillsHeight(float x, float z);

	// 获取地形法线
	DirectX::XMFLOAT3 GetHillsNormal(float x, float z) const;

	// 帧资源
	std::vector<std::unique_ptr<FrameResource>> mFrameResources;

	// 当前帧资源
	FrameResource* mCurrFrameResource = nullptr;

	// 当前帧资源索引
	int mCurrFrameResourceIndex = 0;

	// 描述符的大小
	UINT mCbvSrvDescriptorSize = 0;

	// 根签名
	Microsoft::WRL::ComPtr<ID3D12RootSignature> mRootSignature = nullptr;

	// 模型
	std::unordered_map<std::string, std::unique_ptr<MeshGeometry>> mGeometries;
	
	// 材质
	std::unordered_map<std::string, std::unique_ptr<Material>> mMaterials;

	// 着色器
	std::unordered_map<std::string, Microsoft::WRL::ComPtr<ID3DBlob>> mShaders;

	// 流水线状态对象
	std::unordered_map<std::string, Microsoft::WRL::ComPtr<ID3D12PipelineState>> mPSOs;

	// 输入布局 
	std::vector<D3D12_INPUT_ELEMENT_DESC> mInputLayout;

	// 水的渲染项
	RenderItem* mWavesRitem = nullptr;

	// 所有的渲染项
	std::vector<std::unique_ptr<RenderItem>> mAllRitems;

	// 渲染层
	std::vector<RenderItem*> mRitemLayer[static_cast<int>(RenderLayer::Count)];

	// 波纹
	std::unique_ptr<Waves> mWaves;

	// main pass
	PassConstants mMainPassCB;

	// 观察矩阵
	DirectX::XMFLOAT3 mEyePos = {0.0f, 0.0f, 0.0f};
	DirectX::XMFLOAT4X4 mView = MathHelper::Identity4x4();
	DirectX::XMFLOAT4X4 mProj = MathHelper::Identity4x4();
	float mTheta = 1.5f * DirectX::XM_PI;
	float mPhi = DirectX::XM_PIDIV2 - 0.1f;
	float mRadius = 50.0f;

	// 光源
	float mSunTheta = 1.25f * DirectX::XM_PI;
	float mSunPhi = DirectX::XM_PIDIV4;

	// 鼠标位置
	POINT mLastMousePos{};
};

LitWavesApp::LitWavesApp(const HINSTANCE hInstance) : D3DApp(hInstance)
{
}

LitWavesApp::~LitWavesApp()
{
	if (md3dDevice != nullptr)
		FlushCommandQueue();
}

bool LitWavesApp::Initialize()
{
	// 初始化Windows窗口 和 Direct3D 12
	if (!D3DApp::Initialize()) return false;

	// 重置命令队列
	ThrowIfFailed(mCommandList->Reset(mDirectCmdListAlloc.Get(), nullptr));

	// 获取描述符的大小
	mCbvSrvDescriptorSize = md3dDevice->GetDescriptorHandleIncrementSize(D3D12_DESCRIPTOR_HEAP_TYPE_CBV_SRV_UAV);

	// 产生波浪模拟
	mWaves = std::make_unique<Waves>(128, 128, 1.0f, 0.03f, 4.0f, 0.2f);

	// 创建根签名
	BuildRootSignature();

	// 创建着色器和输入层
	BuildShadersAndInputLayout();

	// 创建陆地模型
	BuildLandGeometry();

	// 创建波浪模型缓冲区
	BuildWavesGeometryBuffers();

	// 创建材质
	BuildMaterials();

	// 创建渲染项
	BuildRenderItems();
	
	// 创建帧资源
	BuildFrameResources();
	
	// 创建流水线状态对象
	BuildPSOs();

	// 初始化命令队列
	ThrowIfFailed(mCommandList->Close());
	ID3D12CommandList* cmdsLists[] = {mCommandList.Get()};
	mCommandQueue->ExecuteCommandLists(_countof(cmdsLists), cmdsLists);

	// 等待命令队列初始化完成
	FlushCommandQueue();

	return true;
}

void LitWavesApp::OnResize()
{
	D3DApp::OnResize();

	// 更改投影矩阵
	const auto p = DirectX::XMMatrixPerspectiveFovLH(0.25f * MathHelper::Pi, AspectRatio(), 1.0f, 1000.0f);
	XMStoreFloat4x4(&mProj, p);
}

void LitWavesApp::Update(const GameTimer& gt)
{
	OnKeyboardInput(gt);
	UpdateCamera(gt);

	// 循环遍历循环帧资源数组
	mCurrFrameResourceIndex = (mCurrFrameResourceIndex + 1) % NumFrameResources;
	mCurrFrameResource = mFrameResources[mCurrFrameResourceIndex].get();

	// 等待GPU处理
	if (mCurrFrameResource->Fence != 0 && mFence->GetCompletedValue() < mCurrFrameResource->Fence)
	{
		const auto eventHandle = CreateEventEx(nullptr, nullptr, false, EVENT_ALL_ACCESS);
		ThrowIfFailed(mFence->SetEventOnCompletion(mCurrFrameResource->Fence, eventHandle));
		WaitForSingleObject(eventHandle, INFINITE);
		CloseHandle(eventHandle);
	}

	// 更新常量缓冲区
	UpdateObjectCBs(gt);
	UpdateMaterialCBs(gt);
	UpdateMainPassCB(gt);
	UpdateWaves(gt);
}

void LitWavesApp::Draw(const GameTimer& gt)
{
	auto cmdListAlloc = mCurrFrameResource->CmdListAlloc;

	// 重置命令队列
	ThrowIfFailed(cmdListAlloc->Reset());
	ThrowIfFailed(mCommandList->Reset(cmdListAlloc.Get(), mPSOs["opaque"].Get()));

	// 更新投影矩阵
	mCommandList->RSSetViewports(1, &mScreenViewport);

	// 更新裁剪矩阵
	mCommandList->RSSetScissorRects(1, &mScissorRect);

	// 设置资源状态
	mCommandList->ResourceBarrier(
		1,
		&CD3DX12_RESOURCE_BARRIER::Transition(CurrentBackBuffer(),
		D3D12_RESOURCE_STATE_PRESENT,
		D3D12_RESOURCE_STATE_RENDER_TARGET));

	// 清空背景缓冲区
	mCommandList->ClearRenderTargetView(
		CurrentBackBufferView(),
		DirectX::Colors::LightSteelBlue,
		0,
		nullptr);

	// 清空模板
	mCommandList->ClearDepthStencilView(
		DepthStencilView(),
		D3D12_CLEAR_FLAG_DEPTH | D3D12_CLEAR_FLAG_STENCIL,
		1.0f,
		0,
	    0,
		nullptr);

	// 指定渲染缓冲区
	mCommandList->OMSetRenderTargets(
		1,
		&CurrentBackBufferView(),
		true,
		&DepthStencilView());

	// 设置根签名
	mCommandList->SetGraphicsRootSignature(mRootSignature.Get());

	// 指定pass
	auto passCB = mCurrFrameResource->PassCB->Resource();
	mCommandList->SetGraphicsRootConstantBufferView(2, passCB->GetGPUVirtualAddress());

	// 绘制所有渲染项
	DrawRenderItems(mCommandList.Get(), mRitemLayer[static_cast<int>(RenderLayer::Opaque)]);

	// 设置资源状态
	mCommandList->ResourceBarrier(
		1,
		&CD3DX12_RESOURCE_BARRIER::Transition(
			CurrentBackBuffer(),
	        D3D12_RESOURCE_STATE_RENDER_TARGET,
	        D3D12_RESOURCE_STATE_PRESENT));

	// 结束命令提交
	ThrowIfFailed(mCommandList->Close());

	// 将命令添加到队列
	ID3D12CommandList* cmdsLists[] = {mCommandList.Get()};
	mCommandQueue->ExecuteCommandLists(_countof(cmdsLists), cmdsLists);

	// 交换前后缓冲区
	ThrowIfFailed(mSwapChain->Present(0, 0));
	mCurrBackBuffer = (mCurrBackBuffer + 1) % SwapChainBufferCount;

	// 设置围栏
	mCurrFrameResource->Fence = ++mCurrentFence;
	mCommandQueue->Signal(mFence.Get(), mCurrentFence);
}

void LitWavesApp::OnMouseDown(WPARAM btnState, const int x, const int y)
{
	mLastMousePos.x = x;
	mLastMousePos.y = y;
	SetCapture(mhMainWnd);
}

void LitWavesApp::OnMouseUp(WPARAM btnState, int x, int y)
{
	ReleaseCapture();
}

void LitWavesApp::OnMouseMove(const WPARAM btnState, const int x, const int y)
{
	// 左键
	if ((btnState & MK_LBUTTON) != 0)
	{
		const auto dx = DirectX::XMConvertToRadians(0.25f * static_cast<float>(x - mLastMousePos.x));
		const auto dy = DirectX::XMConvertToRadians(0.25f * static_cast<float>(y - mLastMousePos.y));
		mTheta += dx;
		mPhi += dy;
		mPhi = MathHelper::Clamp(mPhi, 0.1f, MathHelper::Pi - 0.1f);
	}
	else if ((btnState & MK_RBUTTON) != 0) // 右键
	{
		float dx = 0.2f * static_cast<float>(x - mLastMousePos.x);
		float dy = 0.2f * static_cast<float>(y - mLastMousePos.y);
		mRadius += dx - dy;
		mRadius = MathHelper::Clamp(mRadius, 5.0f, 150.0f);
	}

	mLastMousePos.x = x;
	mLastMousePos.y = y;
}

void LitWavesApp::OnKeyboardInput(const GameTimer& gt)
{
	const auto dt = gt.DeltaTime();

	if (GetAsyncKeyState(VK_LEFT) & 0x8000)
		mSunTheta -= 1.0f * dt;

	if (GetAsyncKeyState(VK_RIGHT) & 0x8000)
		mSunTheta += 1.0f * dt;

	if (GetAsyncKeyState(VK_UP) & 0x8000)
		mSunPhi -= 1.0f * dt;

	if (GetAsyncKeyState(VK_DOWN) & 0x8000)
		mSunPhi += 1.0f * dt;

	mSunPhi = MathHelper::Clamp(mSunPhi, 0.1f, DirectX::XM_PIDIV2);
}

void LitWavesApp::UpdateCamera(const GameTimer& gt)
{
	// 将球面坐标转换为笛卡尔坐标
	mEyePos.x = mRadius * sinf(mPhi) * cosf(mTheta);
	mEyePos.z = mRadius * sinf(mPhi) * sinf(mTheta);
	mEyePos.y = mRadius * cosf(mPhi);

	// 构建视图矩阵
	const auto pos = DirectX::XMVectorSet(mEyePos.x, mEyePos.y, mEyePos.z, 1.0f);
	const auto target = DirectX::XMVectorZero();
	const auto up = DirectX::XMVectorSet(0.0f, 1.0f, 0.0f, 0.0f);
	const auto view = DirectX::XMMatrixLookAtLH(pos, target, up);
	XMStoreFloat4x4(&mView, view);
}

void LitWavesApp::UpdateObjectCBs(const GameTimer& gt)
{
	auto currObjectCB = mCurrFrameResource->ObjectCB.get();
	for (auto& e : mAllRitems)
	{
		if (e->NumFramesDirty > 0)
		{
			const auto world = XMLoadFloat4x4(&e->World);
			ObjectConstants objConstants;
			XMStoreFloat4x4(&objConstants.World, XMMatrixTranspose(world));
			currObjectCB->CopyData(e->ObjCBIndex, objConstants);
			e->NumFramesDirty--;
		}
	}
}

void LitWavesApp::UpdateMaterialCBs(const GameTimer& gt)
{
	auto currMaterialCB = mCurrFrameResource->MaterialCB.get();
	for (auto& e : mMaterials)
	{
		const auto mat = e.second.get();
		if (mat->NumFramesDirty > 0)
		{
			MaterialConstants matConstants;
			matConstants.DiffuseAlbedo = mat->DiffuseAlbedo;
			matConstants.FresnelR0 = mat->FresnelR0;
			matConstants.Roughness = mat->Roughness;
			currMaterialCB->CopyData(mat->MatCBIndex, matConstants);
			mat->NumFramesDirty--;
		}
	}
}

void LitWavesApp::UpdateMainPassCB(const GameTimer& gt)
{
	// 更新投影矩阵
	using DirectX::operator-;
	const auto view = XMLoadFloat4x4(&mView);
	const auto proj = XMLoadFloat4x4(&mProj);
	const auto viewProj = XMMatrixMultiply(view, proj);
	const auto invView = XMMatrixInverse(&XMMatrixDeterminant(view), view);
	const auto invProj = XMMatrixInverse(&XMMatrixDeterminant(proj), proj);
	const auto invViewProj = XMMatrixInverse(&XMMatrixDeterminant(viewProj), viewProj);
	XMStoreFloat4x4(&mMainPassCB.View, XMMatrixTranspose(view));
	XMStoreFloat4x4(&mMainPassCB.InvView, XMMatrixTranspose(invView));
	XMStoreFloat4x4(&mMainPassCB.Proj, XMMatrixTranspose(proj));
	XMStoreFloat4x4(&mMainPassCB.InvProj, XMMatrixTranspose(invProj));
	XMStoreFloat4x4(&mMainPassCB.ViewProj, XMMatrixTranspose(viewProj));
	XMStoreFloat4x4(&mMainPassCB.InvViewProj, XMMatrixTranspose(invViewProj));
	mMainPassCB.EyePosW = mEyePos;
	mMainPassCB.RenderTargetSize = DirectX::XMFLOAT2(static_cast<float>(mClientWidth), static_cast<float>(mClientHeight));
	mMainPassCB.InvRenderTargetSize = DirectX::XMFLOAT2(1.0f / mClientWidth, 1.0f / mClientHeight);
	mMainPassCB.NearZ = 1.0f;
	mMainPassCB.FarZ = 1000.0f;
	mMainPassCB.TotalTime = gt.TotalTime();
	mMainPassCB.DeltaTime = gt.DeltaTime();
	mMainPassCB.AmbientLight = {0.25f, 0.25f, 0.35f, 1.0f};

	// 更新光源位置
	const auto lightDir = -MathHelper::SphericalToCartesian(1.0f, mSunTheta, mSunPhi);

	XMStoreFloat3(&mMainPassCB.Lights[0].Direction, lightDir);
	mMainPassCB.Lights[0].Strength = {1.0f, 1.0f, 0.9f};

	auto currPassCB = mCurrFrameResource->PassCB.get();
	currPassCB->CopyData(0, mMainPassCB);
}

void LitWavesApp::UpdateWaves(const GameTimer& gt) const
{
	// 随机生成波浪
	static auto tBase = 0.0f;
	if (mTimer.TotalTime() - tBase >= 0.25f)
	{
		tBase += 0.25f;
		const auto i = MathHelper::Rand(4, mWaves->RowCount() - 5);
		const auto j = MathHelper::Rand(4, mWaves->ColumnCount() - 5);
		const float r = MathHelper::RandF(0.2f, 0.5f);
		mWaves->Disturb(i, j, r);
	}

	mWaves->Update(gt.DeltaTime());

	// 更新波浪的顶点缓冲区
	auto currWavesVB = mCurrFrameResource->WavesVB.get();
	for (int i = 0; i < mWaves->VertexCount(); ++i)
	{
		Vertex v{};
		v.Pos = mWaves->Position(i);
		v.Normal = mWaves->Normal(i);
		currWavesVB->CopyData(i, v);
	}
	mWavesRitem->Geo->VertexBufferGPU = currWavesVB->Resource();
}

void LitWavesApp::BuildRootSignature()
{
	// 根参数
	CD3DX12_ROOT_PARAMETER slotRootParameter[3];

	// 创建常量缓冲区视图
	for (auto i = 0; i < 3; ++i) slotRootParameter[i].InitAsConstantBufferView(i);

	// 创建根签名描述
	CD3DX12_ROOT_SIGNATURE_DESC rootSigDesc(
		3,
		slotRootParameter,
		0,
		nullptr,
		D3D12_ROOT_SIGNATURE_FLAG_ALLOW_INPUT_ASSEMBLER_INPUT_LAYOUT);

	// 创建根签名指针
	Microsoft::WRL::ComPtr<ID3DBlob> serializedRootSig = nullptr;

	// 错误指针
	Microsoft::WRL::ComPtr<ID3DBlob> errorBlob = nullptr;

	// 序列化根签名
	const auto hr = D3D12SerializeRootSignature(
		&rootSigDesc,
		D3D_ROOT_SIGNATURE_VERSION_1, 
		serializedRootSig.GetAddressOf(),
		errorBlob.GetAddressOf());

	// 输出错误
	if (errorBlob != nullptr)
	{
		OutputDebugStringA(static_cast<char*>(errorBlob->GetBufferPointer()));
	}
	ThrowIfFailed(hr);

	//创建根签名
	ThrowIfFailed(md3dDevice->CreateRootSignature(
		0,
		serializedRootSig->GetBufferPointer(),
		serializedRootSig->GetBufferSize(),
		IID_PPV_ARGS(mRootSignature.GetAddressOf())));
}

void LitWavesApp::BuildShadersAndInputLayout()
{
	// 编译顶点着色器和片元着色器
	mShaders["standardVS"] = d3dUtil::CompileShader(
		L"D:\\Default.hlsl",
		nullptr,
		"VS",
		"vs_5_0");
	mShaders["opaquePS"] = d3dUtil::CompileShader(
		L"D:\\Default.hlsl", 
		nullptr,
		"PS",
		"ps_5_0");

	// 定于着色器输入布局
	mInputLayout =
	{
		{
			"POSITION",
			0,
			DXGI_FORMAT_R32G32B32_FLOAT,
			0,
			0,
			D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
			0
		},
		{
			"NORMAL",
			0,
			DXGI_FORMAT_R32G32B32_FLOAT,
			0,
			12,
			D3D12_INPUT_CLASSIFICATION_PER_VERTEX_DATA,
			0
		}
	};
}

void LitWavesApp::BuildLandGeometry()
{
	// 创建网格
	GeometryGenerator geoGen;
	auto grid = geoGen.CreateGrid(160.0f, 160.0f, 50, 50);

	std::vector<Vertex> vertices(grid.Vertices.size());
	for (size_t i = 0; i < grid.Vertices.size(); ++i)
	{
		auto& p = grid.Vertices[i].Position;
		vertices[i].Pos = p;
		vertices[i].Pos.y = GetHillsHeight(p.x, p.z);
		vertices[i].Normal = GetHillsNormal(p.x, p.z);
	}

	// 顶点缓冲大小
	const UINT vbByteSize = static_cast<UINT>(vertices.size()) * sizeof(Vertex);

	// 索引缓冲大小
	auto indices = grid.GetIndices16();
	const UINT ibByteSize = static_cast<UINT>(indices.size()) * sizeof(std::uint16_t);

	// 构建模型
	auto geo = std::make_unique<MeshGeometry>();
	geo->Name = "landGeo";
	ThrowIfFailed(D3DCreateBlob(vbByteSize, &geo->VertexBufferCPU));
	CopyMemory(geo->VertexBufferCPU->GetBufferPointer(), vertices.data(), vbByteSize);
	ThrowIfFailed(D3DCreateBlob(ibByteSize, &geo->IndexBufferCPU));
	CopyMemory(geo->IndexBufferCPU->GetBufferPointer(), indices.data(), ibByteSize);
	geo->VertexBufferGPU = d3dUtil::CreateDefaultBuffer(
		md3dDevice.Get(),
		mCommandList.Get(),
		vertices.data(),
		vbByteSize,
	    geo->VertexBufferUploader);
	geo->IndexBufferGPU = d3dUtil::CreateDefaultBuffer(
		md3dDevice.Get(),
		mCommandList.Get(),
		indices.data(),
		ibByteSize,
		geo->IndexBufferUploader);
	geo->VertexByteStride = sizeof(Vertex);
	geo->VertexBufferByteSize = vbByteSize;
	geo->IndexFormat = DXGI_FORMAT_R16_UINT;
	geo->IndexBufferByteSize = ibByteSize;

	// 创建子模型
	SubmeshGeometry submesh;
	submesh.IndexCount = static_cast<UINT>(indices.size());
	submesh.StartIndexLocation = 0;
	submesh.BaseVertexLocation = 0;

	geo->DrawArgs["grid"] = submesh;

	mGeometries["landGeo"] = std::move(geo);
}

void LitWavesApp::BuildWavesGeometryBuffers()
{
	// 为每个面创建3个索引
	std::vector<std::uint16_t> indices(3 * mWaves->TriangleCount());
	assert(mWaves->VertexCount() < 0x0000ffff);

	// 创建面
	const auto m = mWaves->RowCount();
	const auto n = mWaves->ColumnCount();
	auto k = 0;
	for (auto i = 0; i < m - 1; ++i)
	{
		for (auto j = 0; j < n - 1; ++j)
		{
			indices[k] = i * n + j;
			indices[k + 1] = i * n + j + 1;
			indices[k + 2] = (i + 1) * n + j;
			indices[k + 3] = (i + 1) * n + j;
			indices[k + 4] = i * n + j + 1;
			indices[k + 5] = (i + 1) * n + j + 1;
			k += 6;
		}
	}

	// 构建动态模型
	const UINT vbByteSize = mWaves->VertexCount() * sizeof(Vertex);
	const UINT ibByteSize = static_cast<UINT>(indices.size()) * sizeof(std::uint16_t);
	auto geo = std::make_unique<MeshGeometry>();
	geo->Name = "waterGeo";
	geo->VertexBufferCPU = nullptr;
	geo->VertexBufferGPU = nullptr;
	ThrowIfFailed(D3DCreateBlob(ibByteSize, &geo->IndexBufferCPU));
	CopyMemory(geo->IndexBufferCPU->GetBufferPointer(), indices.data(), ibByteSize);
	geo->IndexBufferGPU = d3dUtil::CreateDefaultBuffer(
		md3dDevice.Get(),
	    mCommandList.Get(),
		indices.data(),
		ibByteSize,
		geo->IndexBufferUploader);
	geo->VertexByteStride = sizeof(Vertex);
	geo->VertexBufferByteSize = vbByteSize;
	geo->IndexFormat = DXGI_FORMAT_R16_UINT;
	geo->IndexBufferByteSize = ibByteSize;

	// 构建子模型
	SubmeshGeometry submesh;
	submesh.IndexCount = static_cast<UINT>(indices.size());
	submesh.StartIndexLocation = 0;
	submesh.BaseVertexLocation = 0;

	geo->DrawArgs["grid"] = submesh;

	mGeometries["waterGeo"] = std::move(geo);
}

void LitWavesApp::BuildPSOs()
{
	// 创建流水线状态对象描述符
	D3D12_GRAPHICS_PIPELINE_STATE_DESC opaquePsoDesc;
	ZeroMemory(&opaquePsoDesc, sizeof(D3D12_GRAPHICS_PIPELINE_STATE_DESC));
	// 输入布局描述
	opaquePsoDesc.InputLayout = {mInputLayout.data(), static_cast<UINT>(mInputLayout.size())};
	// 根签名指针
	opaquePsoDesc.pRootSignature = mRootSignature.Get();
	// 顶点着色器
	opaquePsoDesc.VS =
	{
		reinterpret_cast<BYTE*>(mShaders["standardVS"]->GetBufferPointer()),
		mShaders["standardVS"]->GetBufferSize()
	};
	// 像素着色器
	opaquePsoDesc.PS =
	{
		reinterpret_cast<BYTE*>(mShaders["opaquePS"]->GetBufferPointer()),
		mShaders["opaquePS"]->GetBufferSize()
	};
	// 光栅器状态
	opaquePsoDesc.RasterizerState = CD3DX12_RASTERIZER_DESC(D3D12_DEFAULT);
	// 混合状态
	opaquePsoDesc.BlendState = CD3DX12_BLEND_DESC(D3D12_DEFAULT);
	// 深度模版缓冲状态
	opaquePsoDesc.DepthStencilState = CD3DX12_DEPTH_STENCIL_DESC(D3D12_DEFAULT);
	// 采样情况掩码
	opaquePsoDesc.SampleMask = UINT_MAX;
	// 图元拓扑类型
	opaquePsoDesc.PrimitiveTopologyType = D3D12_PRIMITIVE_TOPOLOGY_TYPE_TRIANGLE;
	// 同时用的渲染目标个数
	opaquePsoDesc.NumRenderTargets = 1;
	// 渲染目标格式数组
	opaquePsoDesc.RTVFormats[0] = mBackBufferFormat;
	// 采样数量
	opaquePsoDesc.SampleDesc.Count = m4xMsaaState ? 4 : 1;
	// 采样质量
	opaquePsoDesc.SampleDesc.Quality = m4xMsaaState ? (m4xMsaaQuality - 1) : 0;
	// 深度/模版缓冲级别
	opaquePsoDesc.DSVFormat = mDepthStencilFormat;
	// 创建图形流水线状态机
	ThrowIfFailed(md3dDevice->CreateGraphicsPipelineState(&opaquePsoDesc, IID_PPV_ARGS(&mPSOs["opaque"])));
}

void LitWavesApp::BuildFrameResources()
{
	for (auto i = 0; i < NumFrameResources; ++i)
	{
		mFrameResources.push_back(std::make_unique<FrameResource>(
			md3dDevice.Get(),
			1,
			static_cast<UINT>(mAllRitems.size()),
			static_cast<UINT>(mMaterials.size()),
			mWaves->VertexCount()));
	}
}

void LitWavesApp::BuildMaterials()
{
	// 创建草
	auto grass = std::make_unique<Material>();
	grass->Name = "grass";
	grass->MatCBIndex = 0;
	grass->DiffuseAlbedo = DirectX::XMFLOAT4(0.2f, 0.6f, 0.2f, 1.0f);
	grass->FresnelR0 = DirectX::XMFLOAT3(0.01f, 0.01f, 0.01f);
	grass->Roughness = 0.125f;

	// 创建简单水
	auto water = std::make_unique<Material>();
	water->Name = "water";
	water->MatCBIndex = 1;
	water->DiffuseAlbedo = DirectX::XMFLOAT4(0.0f, 0.2f, 0.6f, 1.0f);
	water->FresnelR0 = DirectX::XMFLOAT3(0.1f, 0.1f, 0.1f);
	water->Roughness = 0.0f;

	mMaterials["grass"] = std::move(grass);
	mMaterials["water"] = std::move(water);
}

void LitWavesApp::BuildRenderItems()
{
	// 创建水
	auto wavesRitem = std::make_unique<RenderItem>();
	wavesRitem->World = MathHelper::Identity4x4();
	wavesRitem->ObjCBIndex = 0;
	wavesRitem->Mat = mMaterials["water"].get();
	wavesRitem->Geo = mGeometries["waterGeo"].get();
	wavesRitem->PrimitiveType = D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
	wavesRitem->IndexCount = wavesRitem->Geo->DrawArgs["grid"].IndexCount;
	wavesRitem->StartIndexLocation = wavesRitem->Geo->DrawArgs["grid"].StartIndexLocation;
	wavesRitem->BaseVertexLocation = wavesRitem->Geo->DrawArgs["grid"].BaseVertexLocation;
	mWavesRitem = wavesRitem.get();
	mRitemLayer[static_cast<int>(RenderLayer::Opaque)].push_back(wavesRitem.get());

	// 创建陆地
	auto gridRitem = std::make_unique<RenderItem>();
	gridRitem->World = MathHelper::Identity4x4();
	gridRitem->ObjCBIndex = 1;
	gridRitem->Mat = mMaterials["grass"].get();
	gridRitem->Geo = mGeometries["landGeo"].get();
	gridRitem->PrimitiveType = D3D11_PRIMITIVE_TOPOLOGY_TRIANGLELIST;
	gridRitem->IndexCount = gridRitem->Geo->DrawArgs["grid"].IndexCount;
	gridRitem->StartIndexLocation = gridRitem->Geo->DrawArgs["grid"].StartIndexLocation;
	gridRitem->BaseVertexLocation = gridRitem->Geo->DrawArgs["grid"].BaseVertexLocation;
	mRitemLayer[static_cast<int>(RenderLayer::Opaque)].push_back(gridRitem.get());

	mAllRitems.push_back(std::move(wavesRitem));
	mAllRitems.push_back(std::move(gridRitem));
}

void LitWavesApp::DrawRenderItems(ID3D12GraphicsCommandList* cmdList, const std::vector<RenderItem*>& ritems) const
{
	const auto objCBByteSize = d3dUtil::CalcConstantBufferByteSize(sizeof(ObjectConstants));
	const auto matCBByteSize = d3dUtil::CalcConstantBufferByteSize(sizeof(MaterialConstants));

	auto objectCB = mCurrFrameResource->ObjectCB->Resource();
	auto matCB = mCurrFrameResource->MaterialCB->Resource();

	for (auto ri : ritems)
	{
		cmdList->IASetVertexBuffers(0, 1, &ri->Geo->VertexBufferView());
		cmdList->IASetIndexBuffer(&ri->Geo->IndexBufferView());
		cmdList->IASetPrimitiveTopology(ri->PrimitiveType);

		const auto objCBAddress = objectCB->GetGPUVirtualAddress() + ri->ObjCBIndex * objCBByteSize;
		const auto matCBAddress = matCB->GetGPUVirtualAddress() + ri->Mat->MatCBIndex * matCBByteSize;

		cmdList->SetGraphicsRootConstantBufferView(0, objCBAddress);
		cmdList->SetGraphicsRootConstantBufferView(1, matCBAddress);

		cmdList->DrawIndexedInstanced(ri->IndexCount, 1, ri->StartIndexLocation, ri->BaseVertexLocation, 0);
	}
}

float LitWavesApp::GetHillsHeight(const float x, const float z)
{
	return 0.3f * (z * sinf(0.1f * x) + x * cosf(0.1f * z));
}

DirectX::XMFLOAT3 LitWavesApp::GetHillsNormal(const float x, const float z) const
{
	// n = (-df/dx, 1, -df/dz)
	DirectX::XMFLOAT3 n(
		-0.03f * z * cosf(0.1f * x) - 0.3f * cosf(0.1f * z),
		1.0f,
		-0.3f * sinf(0.1f * x) + 0.03f * x * sinf(0.1f * z));
	const auto unitNormal = DirectX::XMVector3Normalize(XMLoadFloat3(&n));
	XMStoreFloat3(&n, unitNormal);
	return n;
}

//主程序
int WINAPI WinMain(const HINSTANCE hInstance, HINSTANCE, PSTR, int)
{
	// 检测内存泄露
#if defined(DEBUG) | defined(_DEBUG)
	_CrtSetDbgFlag(_CRTDBG_ALLOC_MEM_DF | _CRTDBG_LEAK_CHECK_DF);
#endif
	try
	{
		LitWavesApp theApp(hInstance);
		if (!theApp.Initialize())
			return 0;

		return theApp.Run();
	}
	catch (DxException& e)
	{
		MessageBox(nullptr, e.ToString().c_str(), L"HR Failed", MB_OK);
		return 0;
	}
}
