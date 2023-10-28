#!/bin/bash

TIMES=1000
TIMEFILE="time_results.txt"

> $TIMEFILE

for i in $(seq 1 $TIMES); do
    (echo "1" | time ./bin/repl > /dev/null) 2>> $TIMEFILE
done


awk '/user/ { sum += $2 } END { printf "Average user time: %.2f\n", sum/NR }' $TIMEFILE

