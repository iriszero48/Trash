import 'dart:math';

final ROW = 128;
final COL = 128;
final SIZE = ROW * COL;

// print(a[0]);

add(mat, val) {
    for (var i = 0; i < SIZE; i++) mat[i] += val;
}

div(mat, val) {
    for (var i = 0; i < SIZE; i++) mat[i] /= val;
}

mul(a, b, c) {
    for (var row = 0; row < 128; row++) {
        for (var col = 0; col < 128; col++) {
            var v = 0.0;
            for (var i = 0; i < 128; i++) {
                v += a[row * 128 + i] * b[i * 128 + col];
            }
            c[row * 128 + col] = v;
        }
    }
}

main() {
    // var a = List<double>.generate(SIZE, (i) => (i + 1.0) / SIZE);
    var a = List<double>.generate(SIZE, (i) => Random().nextDouble());
    a[127 * 128 + 127] = 1.0;
    var b = List<double>.filled(SIZE, 1.0);
    var c = List<double>.filled(SIZE, 0.0);
    var sum_value = 0.0;

    final tp1 = DateTime.now();

    for (var i = 0; i < 1000; i++) {
        add(b, i);
        mul(a, b, c);

        add(b, i + 1);
        mul(c, b, a);

        div(a, c[127 * COL + 127]);
    }

    for (var i = 0; i < SIZE; i++) sum_value += a[i];

    final tp2 = DateTime.now();
    final dur = (tp2.microsecondsSinceEpoch - tp1.microsecondsSinceEpoch) / 1000.0;

    print("$dur $sum_value");
}