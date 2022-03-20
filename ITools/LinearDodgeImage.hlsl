Texture2D BufferIn : register(t0);
Texture2D Ref : register(t1);
RWTexture2D<float4> BufferOut : register(u0);

[numthreads(32, 32, 1)]
void LinearDodgeImage( uint3 DTid : SV_DispatchThreadID )
{
    BufferOut[DTid.xy] = BufferIn[DTid.xy] + Ref[DTid.xy];
}
