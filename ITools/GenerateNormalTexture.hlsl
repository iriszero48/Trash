struct ShaderData
{
    float Bias;
    uint InvertR;
    uint InvertG;
    float Width;
    float Height;
};

Texture2D<float4> BufferIn : register(t0);
StructuredBuffer<ShaderData> Data : register(t1);

SamplerState TexSampler : register(s0);

RWTexture2D<float4> BufferOut : register(u0);

[numthreads(32, 32, 1)]
void GenerateNormalTexture(uint3 GID : SV_GroupID, uint3 DTID : SV_DispatchThreadID, uint3 GTID : SV_GroupThreadID, int GI : SV_GroupIndex)
{
    ShaderData d = Data[0];

    float stepW = 1. / d.Width;
    float stepH = 1. / d.Height;
    float2 pos = DTID.xy / float2(d.Width, d.Height);

    float d0 = abs(BufferIn.SampleLevel(TexSampler, pos                         , 0).r);
    float d1 = abs(BufferIn.SampleLevel(TexSampler, pos + float2( stepW,      0), 0).r);
    float d2 = abs(BufferIn.SampleLevel(TexSampler, pos + float2(-stepW,      0), 0).r);
    float d3 = abs(BufferIn.SampleLevel(TexSampler, pos + float2(     0, -stepH), 0).r);
    float d4 = abs(BufferIn.SampleLevel(TexSampler, pos + float2(     0,  stepH), 0).r);

    float dx = ((d2 - d0) + (d0 - d1)) * 0.5;
    float dy = ((d4 - d0) + (d0 - d3)) * 0.5;

    dx = dx * (d.InvertR ? -1. : 1.);
    dy = dy * (d.InvertG ? -1. : 1.);
    float dz = 1. - ((d.Bias - 0.1) / 100.);

    BufferOut[DTID.xy].rgba = float4(normalize(float3(dx, dy, dz)).xyz * 0.5 + 0.5, 1.0);
}
