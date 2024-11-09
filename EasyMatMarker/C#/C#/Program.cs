const int ROW = 128;
const int COL = 128;
const int SIZE = ROW * COL;

// var a = Enumerable.Range(0, SIZE).Select(i => (i + 1.0) / SIZE).ToArray();
var a = Enumerable.Range(0, SIZE).Select(i => new Random(DateTime.Now.Millisecond).NextDouble()).ToArray();
a[128 * 128 - 1] = 1;
var b = Enumerable.Range(0, SIZE).Select(_ => 1.0).ToArray();
var c = new double[SIZE];
var sumValue = 0.0;

static void Add(double[] mat, double val)
{
    for (int i = 0; i < SIZE; i++) mat[i] += val;
};

static void Div(double[] mat, double val)
{
    for (int i = 0; i < SIZE; i++) mat[i] /= val;
};

static void Mul(double[] a, double[] b, double[] c)
{
    for (int row = 0; row < ROW; row++)
    {
        for (int col = 0; col < COL; col++)
        {
            var v = 0.0;
            for (int i = 0; i < COL; i++)
            {
                v += a[row * COL + i] * b[i * COL + col];
            }
            c[row * COL + col] = v;
        }
    }
};

var tp1 = DateTime.Now;

for (int i = 0; i < 1000; i++) 
{
    Add(b, i);
    Mul(a, b, c);

    Add(b, i + 1);
    Mul(c, b, a);

    Div(a, c[127 * COL + 127]);
}

for (int i = 0; i < SIZE; ++i) sumValue += a[i];

var tp2 = DateTime.Now;

var dur = tp2.Subtract(tp1).TotalNanoseconds / 1000.0 / 1000.0;

Console.WriteLine($"{dur} {sumValue}");
