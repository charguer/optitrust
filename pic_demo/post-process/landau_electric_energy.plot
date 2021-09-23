set terminal pdf color
set output "landau_electric_energy.pdf"
set autoscale

set xlabel "Time"
set xtic auto

set ylabel "Energy = 0.5 integral (E_x^2 + E_y^2 + E_z^2)"
set logscale y
set ytic auto

plot \
  "../3d_runs/run1/diag_lee_8corners.txt" using 1:(0.5*$4) title 'Simulated' with lines, \
  0.5 * exp(2.0 * (2.85 - 0.0084664151303061653770 * x)) title 'Theory, first time mode' with lines

