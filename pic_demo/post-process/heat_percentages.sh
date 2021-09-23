#!/bin/bash

echo "Proportion fast particles | Drift velocity | Thermal speed | 1 cell away | 2 cells away | 3 cells away | Shared bags | Timings" > heat_percentages.txt

add_a_test_case() {
    printf "$1\t$2\t$3\t" >> heat_percentages.txt
    
    grep -r " of the particles moved 1 cell away." "../3d_heat_instrum_runs/proportion$1-drift$2-vth$3/std_output.txt" > tmp.txt
    sed -i 's| of the particles moved 1 cell away.||g' tmp.txt
    if [ -s tmp.txt ]
    then
        cat tmp.txt | tr -d "\n" >> heat_percentages.txt
    else
        printf "0.00000%%" >> heat_percentages.txt
    fi
    printf "\t" >> heat_percentages.txt
    grep -r " of the particles moved 2 cells away." "../3d_heat_instrum_runs/proportion$1-drift$2-vth$3/std_output.txt" > tmp.txt
    sed -i 's| of the particles moved 2 cells away.||g' tmp.txt
    if [ -s tmp.txt ]
    then
        cat tmp.txt | tr -d "\n" >> heat_percentages.txt
    else
        printf "0.00000%%" >> heat_percentages.txt
    fi
    printf "\t" >> heat_percentages.txt
    grep -r " of the particles moved 3 cells away." "../3d_heat_instrum_runs/proportion$1-drift$2-vth$3/std_output.txt" > tmp.txt
    sed -i 's| of the particles moved 3 cells away.||g' tmp.txt
    if [ -s tmp.txt ]
    then
        cat tmp.txt | tr -d "\n" >> heat_percentages.txt
    else
        printf "0.00000%%" >> heat_percentages.txt
    fi
    printf "\t" >> heat_percentages.txt
    
    grep -r "Number of particles in shared bags : " "../3d_heat_instrum_runs/proportion$1-drift$2-vth$3/std_output.txt" > tmp.txt
    sed -i 's|.*on average (||g' tmp.txt
    sed -i 's|).*||g' tmp.txt
    cat tmp.txt >> heat_percentages.txt
}

add_a_test_case 0.00 0.00 0.339
add_a_test_case 0.02 11.0 0.126
add_a_test_case 0.02 13.0 0.178
add_a_test_case 0.02 15.0 0.234
add_a_test_case 0.02 17.0 0.287
add_a_test_case 0.02 20.0 0.355
add_a_test_case 0.02 22.0 0.3585

rm tmp.txt
