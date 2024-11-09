#include <assert.h>
#include <stdio.h>
#include <time.h>
#include <stdlib.h>

#define ROW 128
#define COL 128
#define SIZE (ROW * COL)

#define MatMul(_a, _b, _c)                                  \
    for (int row = 0; row < ROW; ++row)                     \
    {                                                       \
        for (int col = 0; col < COL; ++col)                 \
        {                                                   \
            double v = 0;                                   \
            for (int i = 0; i < COL; ++i)                   \
            {                                               \
                v += _a[row * 128 + i] * _b[i * 128 + col]; \
            }                                               \
            _c[row * 128 + col] = v;                        \
        }                                                   \
    }

static double GetRandom(double min, double max)
{
    return ((max - min) * (rand() / RAND_MAX)) + min;
}

static double ToNanos(struct timespec *ts)
{
    return (double)ts->tv_sec * 1000000000. + ts->tv_nsec;
}

static void MatAdd(double *mat, double value)
{
    for (int i = 0; i < SIZE; ++i)
        mat[i] += value;
}

static void MatDiv(double *mat, double value)
{
    for (int i = 0; i < SIZE; ++i)
        mat[i] /= value;
}

int main()
{
    double a[SIZE];
    double b[SIZE];
    double c[SIZE];

    // for (int i = 0; i < SIZE; ++i) a[i] = (i + 1.) / SIZE;
    for (int i = 0; i < SIZE; ++i) a[i] = i == SIZE - 1 ? 1.0 : GetRandom(0, 1);
    for (int i = 0; i < SIZE; ++i) b[i] = 1.;

    double sum_value = 0;

    struct timespec tp1, tp2;
    timespec_get(&tp1, TIME_UTC);

    for (int offset = 0; offset < 1000; ++offset)
    {
        MatAdd(b, offset);
        {
            MatMul(a, b, c);
        }

        MatAdd(b, offset + 1);
        {
            MatMul(c, b, a);
        }

        MatDiv(a, c[127 * 128 + 127]);
    }

    for (int i = 0; i < SIZE; ++i)
        sum_value += a[i];

    timespec_get(&tp2, TIME_UTC);

    printf("%g %g", (ToNanos(&tp2) - ToNanos(&tp1)) / 1000. / 1000., sum_value);
}
