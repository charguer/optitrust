set terminal pdf color
set output "muschietti_electric_energy.pdf"
set autoscale

set xlabel "Time"
set xtic auto

set ylabel "0.5 sqrt(integral(E_x^2 + E_y^2) - Fourier modes(k,0))"
set ytic auto
set format y '%g'

plot \
  "../2d3v_runs/run1/diag_energy.txt" using 1:(0.5*sqrt($2+$3-($4+$5+$6+$7+$8+$9+$10+$11+$12+$13+$14+$15+$16+$17+$18+$19+$20+$21+$22+$23))) title 'Simulated' with lines

