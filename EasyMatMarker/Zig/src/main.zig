const std = @import("std");

const ROW = 128;
const COL = 128;
const SIZE = ROW * COL;

const Mat = [SIZE]f64;

pub fn add(mat: *Mat, val: f64) !void {
    for (mat) |*x| {
        x.* += val;
    }
}

pub fn div(mat: *Mat, val: f64) !void {
    for (mat) |*x| {
        x.* /= val;
    }
}

pub fn mul(a: Mat, b: Mat, c: *Mat) !void {
    for (0..ROW) |row| {
        for (0..COL) |col| {
            var v: f64 = 0.0;
            for (0..COL) |i| {
                v += a[row * 128 + i] * b[i * 128 + col];
            }
            c[row * 128 + col] = v;
        }
    }
}

pub fn main() !void {
    var a: Mat = undefined;
    var b: Mat = undefined;
    var c: Mat = undefined;
    var sum_value: f64 = 0.0;

    for (0..SIZE) |i| {
        // a[i] = @as(f64, @floatFromInt(i + 1)) / SIZE;
        a[i] = std.crypto.random.float(f64);
    }
    a[128 * 128 - 1] = 1.0;
    for (0..SIZE) |i| b[i] = 1;

    const tp1 = std.time.nanoTimestamp();

    for (0..1000) |i| {
        try add(&b, @floatFromInt(i));
        try mul(a, b, &c);

        try add(&b, @as(f64, @floatFromInt(i)) + 1.0);
        try mul(c, b, &a);

        try div(&a, c[127 * COL + 127]);
    }

    for (0..SIZE) |i| {
        sum_value += a[i];
    }

    const tp2 = std.time.nanoTimestamp();

    const stdout = std.io.getStdOut().writer();
    try stdout.print("{d} {d}\n", .{ @as(f64, @floatFromInt(tp2 - tp1)) / 1000.0 / 1000.0, sum_value });
}
