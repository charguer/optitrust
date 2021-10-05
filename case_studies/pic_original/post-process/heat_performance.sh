#!/bin/bash

echo "Proportion fast particles | Drift velocity | Thermal speed | Timings" > heat_performance.txt

add_a_test_case() {
    printf "$1\t$2\t$3\t" >> heat_performance.txt
    
    grep -r "Execution time (total)    : " "../3d_heat_runs/proportion$1-drift$2-vth$3/std_output.txt" > tmp.txt
    sed -i 's|Execution time (total)    : ||g' tmp.txt
    sed -i 's| s||g' tmp.txt
    cat tmp.txt >> heat_performance.txt
}

add_a_test_case 0.00 0.00 0.339
add_a_test_case 0.02 11.0 0.126
add_a_test_case 0.02 13.0 0.178
add_a_test_case 0.02 15.0 0.234
add_a_test_case 0.02 17.0 0.287
add_a_test_case 0.02 20.0 0.355
add_a_test_case 0.02 22.0 0.3585

rm tmp.txt
