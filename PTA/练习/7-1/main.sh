read n;
m=`printf "%-$((n+1))s\n" "1" | sed 's/\s/0/g'`;
for i in `seq 1 $m`;
do
    sum=0;
    for j in `seq 0 $((${#i}-1))`; do k=${i:$j:1}; sum=$((sum+k*k*k*k*k)); done
    if [ $sum -eq $i ]; then echo $i; fi
done
