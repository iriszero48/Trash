#!/usr/bin/env wolframscript
(* a = Array[N[# / (128 * 128)] &, 128 * 128]; *)
a = Array[ N[ If[# == 128 * 128, 1, RandomReal[] ] ] &, 128 * 128];
b = Table[1, 128 * 128];
c = Table[1, 128 * 128];

tp1 = AbsoluteTime[];

Do[
    b += i - 1;
    Do[
        Do[
            c[[row * 128 + col + 1]] = N[ Sum[a[[row * 128 + j + 1]] * b[[j * 128 + col + 1]], {j, 0, 128 - 1}] ];
        , {col, 0, 128 - 1}];
    , {row, 0, 128 - 1}];

    b += i;
    Do[
        Do[
            a[[row * 128 + col + 1]] = N[ Sum[c[[row * 128 + j + 1]] * b[[j * 128 + col + 1]], {j, 0, 128 - 1}] ];
        , {col, 0, 128 - 1}];
    , {row, 0, 128 - 1}];

    a /= c[[128*128]];
, {i, 1}];

sumVal = Total[a];

tp2 = AbsoluteTime[];

Print[N[tp2 - tp1] * 1000, " ", sumVal];
