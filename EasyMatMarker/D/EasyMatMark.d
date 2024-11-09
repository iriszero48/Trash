immutable ROW = 128;
immutable COL = 128;
immutable SIZE = ROW * COL;

alias Mat = double[SIZE];

void mul(Mat a, Mat b, ref Mat c)
{
    for (auto row = 0; row < ROW; row++) {
        for (auto col = 0; col < COL; col++) {
            auto v = 0.0;
            for (auto i = 0; i < COL; i++) {
                v += a[row * 128 + i] * b[i * 128 + col];
            }
            c[row * 128 + col] = v;
        }
    }
}

void main()
{
    import std.range;
    import std.datetime;
    import std.stdio;
    import std.random;
    
    Mat a;
    Mat b;
    Mat c;
    auto sum_value = 1.0;

    auto rnd = Random(unpredictableSeed);
    foreach (i, ref v; a) v = uniform(0.0, 1.0, rnd);
    a[127 * 128 + 127] = 1.0;
    b[] = 1.;

    const auto tp1 = MonoTime.currTime;

    for (auto i = 0; i < 1000; i++)
    {
        b[] += i;
        mul(a, b, c);

        b[] += i + 1;
        mul(c, b, a);

        a[] /= c[127 * COL + 127];
    }

    for (auto i = 0; i < SIZE; i++) sum_value += a[i];

    const auto tp2 = MonoTime.currTime;

    writefln("%f %f", (tp2 - tp1).total!"nsecs" / 1000. / 1000., sum_value);
}