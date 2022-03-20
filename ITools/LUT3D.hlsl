struct ShaderData
{
    float3 DMax;
    float3 DMin;
    float Size;
};

Texture2D<float4> BufferIn : register(t0);
Texture3D<float3> Cube : register(t1);
StructuredBuffer<ShaderData> Data : register(t2);

SamplerState TexSampler : register(s0);

RWTexture2D<float4> BufferOut : register(u0);

[numthreads(32, 32, 1)]
void LUT3D(uint3 GID : SV_GroupID, uint3 DTID : SV_DispatchThreadID, uint3 GTID : SV_GroupThreadID, int GI : SV_GroupIndex)
{
    ShaderData d = Data[0];
    float3 c0 = BufferIn[DTID.xy].rgb;
    float3 c1 = (c0 - d.DMin) / (d.DMax - d.DMin);
    float3 c2 = c1.bgr;
    BufferOut[DTID.xy].rgba = float4(Cube.SampleLevel(TexSampler, c2, 0), 1.0);
}
