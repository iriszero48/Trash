Texture2D BufferIn : register(t0);
StructuredBuffer<float4> Data : register(t1);

RWTexture2D<float4> BufferOut : register(u0);

[numthreads(32, 32, 1)]
void LinearDodgeColor(uint3 GID : SV_GroupID, uint3 DTID : SV_DispatchThreadID, uint3 GTID : SV_GroupThreadID, int GI : SV_GroupIndex)
{
    BufferOut[DTID.xy] = BufferIn[DTID.xy] + Data[0];
}
