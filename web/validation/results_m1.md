<!-- Written by tests/run_tests_m1.js (2026-11-21T13:55:35.210Z); Deno 1.46.3, V8 12.9.202.5-rusty; vectors of walkbench 0.7.0, commit 754de72b70677dc048b3d7949322e79cf7704546 -->

| Test | Check | Largest difference | Tolerance | Result |
|---|---|---|---|---|
| T1 | fixed point (theta, theta_dot) and period, against walkbench at rtol 1e-13 | 1.19e-12 | 1.00e-9 | pass |
| T1b | fixed point and period, against walkbench at its default tolerances (information) | 1.93e-9 | information |  |
| T2 | eigenvalue moduli of the linearized step map (linearize()) | 8.14e-9 | 1.00e-4 | pass |
| T2b | eigenvalue moduli, against the forward differences of fixed_point() (information) | 1.37e-7 | information |  |
| T2c | J, b, a, e and static gain of linearize() (information) | 2.31e-6 | information |  |
| T3 | ARFIMA inputs from the normal draws of R (5 series, d from -0.30 to 0.45) | 1.14e-16 | 1.00e-9 | pass |
| T4 | fixed series: rescaling and circular start | 4.34e-19 | 1.00e-9 | pass |
| T5 | run of 620 steps: step durations, against walkbench at rtol 1e-13 | 1.54e-11 | 1.00e-7 | pass |
| T5f | run of 620 steps: start and end times of the steps, cumulated over the run, against walkbench at rtol 1e-13 | 3.49e-10 | 1.00e-7 | pass |
| T5g | run of 620 steps: durations and cumulated times, against walkbench at its default tolerances (information) | 2.12e-7 | information |  |
| T5b | run of 620 steps: post-strike states (theta, theta_dot) | 9.46e-11 | 1.00e-7 | pass |
| T5c | run of 620 steps: no fall, same number of steps, of samples and same legs (steps 620 / 620, samples 80207 / 80207) | 0 | 0 | pass |
| T5d | run of 620 steps: sample times of steps 1, 2, 310, 311, 619, 620 | 4.55e-13 | 1.00e-9 | pass |
| T5e | run of 620 steps: sampled states of the same steps, against walkbench at rtol 1e-13 | 1.28e-10 | 1.00e-7 | pass |
| T5h | run of 620 steps: sampled states of the same steps, against walkbench at its default tolerances (information) | 7.83e-8 | information |  |
| T6 | run that falls (seed 1001, sd 0.006): same step of the fall (fall at step 40 / 40 (stance angle reached 1.2 rad in absolute value)) | 0 | 0 | pass |
| T6b | run that falls: durations of the steps before the fall | 2.10e-9 | 1.00e-7 | pass |
| T7 | short run, settings of the plan: divergence curve, smoothed curve and local slopes | 2.96e-14 | 1.00e-9 | pass |
| T7b | short run, settings of the plan: lambda_S and lambda_L (per time unit and per step) (lambda_S 0.42651805157206313, lambda_L 0.020684163798008134) | 9.30e-16 | 1.00e-9 | pass |
| T7c | short run, settings of the plan: same neighbors, number of pairs, horizon and Theiler window (neighbors differing 0; pairs 13306 / 13306; horizon 517 / 517; Theiler 259 / 259) | 0 | 0 | pass |
| T8 | short run, windows 0-0.2 and 4-6 steps: divergence curve, smoothed curve and local slopes | 2.96e-14 | 1.00e-9 | pass |
| T8b | short run, windows 0-0.2 and 4-6 steps: lambda_S and lambda_L (per time unit and per step) (lambda_S 0.4553203334375885, lambda_L 0.019695488685622407) | 0 | 1.00e-9 | pass |
| T8c | short run, windows 0-0.2 and 4-6 steps: same neighbors, number of pairs, horizon and Theiler window (neighbors differing 0; pairs 12659 / 12659; horizon 776 / 776; Theiler 259 / 259) | 0 | 0 | pass |
| T9 | DFA alpha on the values of R (5 series, one too short) | 2.78e-16 | 1.00e-9 | pass |
| T10 | end to end: lambda_S and lambda_L (per time unit and per step) (JavaScript lambda_S 0.6014767756309833, lambda_L 0.03067455406389653; walkbench 0.601469447589853, 0.030680159432545772) | 2.84e-5 | 1.00e-4 | pass |
| T10b | end to end: alpha of the input, of the command, of the step and stride durations | 1.83e-10 | 1.00e-6 | pass |
| T10c | end to end: mean, standard deviation and coefficient of variation of the step durations | 3.42e-10 | 1.00e-7 | pass |
| T10d | end to end: number of complete pairs (information) (76804 / 76803) | 1.00e+0 | information |  |
| T11 | short run: durations of the kept steps (samples 14231 / 14231) | 1.38e-9 | 1.00e-7 | pass |
| T12 | walkbench at its default tolerances against walkbench at rtol 1e-13: period (information) | 1.94e-9 | information |  |
| T12b | walkbench at its default tolerances against walkbench at rtol 1e-13: end time of the run (information) | 2.12e-7 | information |  |
| T13 | generator of the page: uniform draws of runif() after set.seed() (5 seeds) | 0 | 1.00e-9 | pass |
| T13b | generator of the page: normal draws of rnorm() after set.seed() (5 seeds, 1000 draws each) | 8.88e-16 | 1.00e-9 | pass |
| T13c | input_arfima() from the seed alone, generator of the page (5 series) | 1.14e-16 | 1.00e-9 | pass |
