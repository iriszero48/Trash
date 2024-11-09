#!/usr/bin/zsh
zmodload zsh/datetime

a=()
b=()
c=()
sumValue=$(( 0.0 ))

# for i in {1..16384}; do a+=( $(( i/16384. )) ); done
for i in {1..16384}; do a+=( $(( RANDOM/32767. )) ); done
a[16384]=$(( 1. ))
for i in {1..16384}; do b+=( $(( 1.0 )) ); done
for i in {1..16384}; do c+=( $(( 0.0 )) ); done

tp1=$EPOCHREALTIME

for i in {1..50}; do
    i1=$(( i-1 ))
    for bi in {1..16384}; do b[bi]=$(( b[bi]+i1 )); done
    for row in {0..127}; do
        for col in {0..127}; do
            v=$(( 0.0 ))
            idx0=$(( row*128+col+1 ))
            for vi in {0..127}; do v=$(( v+a[row*128+vi+1]*b[vi*128+col+1] )); done
            c[idx0]=$v
        done
    done

    for bi in {1..16384}; do b[bi]=$(( b[bi]+i )); done
    for row in {0..127}; do
        for col in {0..127}; do
            v=$(( 0.0 ))
            idx0=$(( row*128+col+1 ))
            for vi in {0..127}; do v=$(( v+c[row*128+vi+1]*b[vi*128+col+1] )); done
            a[idx0]=$v
        done
    done

    dv=$(( c[16384] ))
    for ai in {1..16384}; do a[ai]=$(( a[ai]/dv )); done
done

for x in $a; do sumValue=$(( sumValue+x )); done

tp2=$EPOCHREALTIME

print "$(( (tp2-tp1)*1000. )) $sumValue"