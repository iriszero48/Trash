Texture2D BufferIn : register(t0);

RWTexture2D<float4> BufferOut : register(u0);

[numthreads(32, 32, 1)]
void NormalMapConvertorRGB2DA(uint3 GID : SV_GroupID, uint3 DTID : SV_DispatchThreadID, uint3 GTID : SV_GroupThreadID, int GI : SV_GroupIndex)
{
    float2 p = BufferIn[DTID.xy].rg;
    BufferOut[DTID.xy] = float4(p.y, p.y, p.y, p.x);
}
