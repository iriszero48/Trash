% a = [1:128*128]/(128*128);
a = rand(1, 128 * 128);
b = reshape(ones(128), 1, 128 * 128);
c = reshape(zeros(128), 1, 128 * 128);

tp1 = time();

for loop_idx = 1:1,
    add_v = loop_idx - 1.0;
    for idx = 1:128*128,
        b(idx) = b(idx) + add_v;
    end;
    for row = 0:127,
        for col = 0:127,
            v = 0;
            for idx = 0:127,
                v += a(row * 128 + idx + 1) * b(idx * 128 + col + 1);
            end;
            c(row * 128 + col + 1) = v;
        end;
    end;

    for idx = 1:128*128,
        b(idx) = b(idx) + loop_idx;
    end;
    for row = 0:127,
        for col = 0:127,
            v = 0;
            for idx = 0:127,
                v += c(row * 128 + idx + 1) * b(idx * 128 + col + 1);
            end;
            a(row * 128 + col + 1) = v;
        end;
    end;

    div_v = c(128 * 128);
    for idx = 1:128*128,
        a(idx) = a(idx) / div_v;
    end;
end;

sum_v = 0.0;
for idx = 1:128*128,
    sum_v += a(idx);
end;

tp2 = time();

printf("%g %g\n", (tp2 - tp1) * 1000, sum_v);
