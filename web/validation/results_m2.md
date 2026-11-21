<!-- Written by tests/run_tests_m2.js (2026-11-21T13:55:39.070Z); Deno 1.46.3, V8 12.9.202.5-rusty; vectors of walkbench 0.7.0, commit dee218f2abaeedeb4dee936993093798ab1ac05a -->

| Test | Check | Largest difference | Tolerance | Result |
|---|---|---|---|---|
| T1 | fixed point (4 state variables) and period, against walkbench at rtol 1e-13 | 1.53e-12 | 1.00e-9 | pass |
| T1b | fixed point and period, against walkbench at its default tolerances (information) | 7.38e-10 | information |  |
| T1c | fixed point reached by the continuation of the README of walkbench, against walkbench at rtol 1e-13 | 1.57e-12 | 1.00e-9 | pass |
| T1d | fixed point of the starting configuration (r_f = 0, k = 0) from the default guess, against walkbench at its default tolerances (information) | 2.11e-9 | information |  |
| T2 | eigenvalue moduli of the linearized step map (linearize()) | 6.72e-8 | 1.00e-4 | pass |
| T2b | eigenvalue moduli, against the forward differences of fixed_point() (information) | 4.82e-6 | information |  |
| T2c | transient of the analysis plan (smallest n with mu^(2n) <= 0.005) (34 / 34 steps) | 0 | 0 | pass |
| T5 | run of 634 steps: step durations, against walkbench at rtol 1e-13 | 6.88e-12 | 1.00e-7 | pass |
| T5f | run of 634 steps: start and end times of the steps, cumulated over the run, against walkbench at rtol 1e-13 | 5.29e-10 | 1.00e-7 | pass |
| T5g | run of 634 steps: durations and cumulated times, against walkbench at its default tolerances (information) | 4.17e-7 | information |  |
| T5b | run of 634 steps: post-strike states (4 state variables) | 1.89e-10 | 1.00e-7 | pass |
| T5c | run of 634 steps: no fall, same number of steps, of samples and same legs (steps 634 / 634, samples 28397 / 28397) | 0 | 0 | pass |
| T5d | run: sample times of steps 1, 2, 317, 318 and of the last two steps | 5.68e-14 | 1.00e-9 | pass |
| T5e | run: sampled states of the same steps, against walkbench at rtol 1e-13 | 1.05e-9 | 1.00e-7 | pass |
| T5h | run: sampled states of the same steps, against walkbench at its default tolerances (information) | 8.23e-7 | information |  |
| T6 | run that falls (seed 2001, d 0.45, sd 0.00045): same step of the fall (fall at step 289 / 289 (stance angle reached 1.2 rad in absolute value)) | 0 | 0 | pass |
| T6b | run that falls: durations of the steps before the fall | 7.89e-8 | 1.00e-7 | pass |
| T7r | short run, settings of the plan: resampled state variables of the model (global grid, interpolation inside each step) (5501 / 5501 rows) | 0 | 1.00e-9 | pass |
| T7 | short run, settings of the plan: divergence curve | 8.88e-16 | 1.00e-9 | pass |
| T7b | short run, settings of the plan: lambda_S and lambda_L (per stride and per step) (lambda_S 1.942315280970824, lambda_L 0.04851315582703474) | 2.44e-15 | 1.00e-9 | pass |
| T7c | short run, settings of the plan: same neighbors, number of pairs, horizon and Theiler window (neighbors differing 0; pairs 3969 / 3969; horizon 1000 / 1000; Theiler 50 / 50) | 0 | 0 | pass |
| T8 | short run, windows 0-1 and 3-8 strides: divergence curve | 8.88e-16 | 1.00e-9 | pass |
| T8b | short run, windows 0-1 and 3-8 strides: lambda_S and lambda_L (per stride and per step) (lambda_S 1.4326524857543874, lambda_L 0.019862030786545647) | 1.63e-16 | 1.00e-9 | pass |
| T8c | short run, windows 0-1 and 3-8 strides: same neighbors, number of pairs, horizon and Theiler window (neighbors differing 0; pairs 3969 / 3969; horizon 1000 / 1000; Theiler 50 / 50) | 0 | 0 | pass |
| T10e | end to end: number of points of the resampling grid, page and grid_count() of walkbench (JavaScript 30001; walkbench 30001) | 0 | 0 | pass |
| T10 | end to end: lambda_S and lambda_L (per stride and per step) (JavaScript lambda_S 2.4853439850702177, lambda_L 0.07403090635242442; walkbench 2.4853437291885205, 0.07403090979616786) | 2.56e-7 | 1.00e-4 | pass |
| T10b | end to end: alpha of the input, of the command, of the step and stride durations | 3.77e-9 | 1.00e-6 | pass |
| T10c | end to end: mean, standard deviation and coefficient of variation of the step durations | 6.57e-10 | 1.00e-7 | pass |
| T10d | end to end: number of complete pairs (information) (28040 / 28040) | 0 | information |  |
| T11 | short run: durations of the kept steps (samples 4926 / 4926) | 1.05e-9 | 1.00e-7 | pass |
| T12 | walkbench at its default tolerances against walkbench at rtol 1e-13: period (information) | 7.37e-10 | information |  |
| T12b | walkbench at its default tolerances against walkbench at rtol 1e-13: end time of the run (information) | 4.16e-7 | information |  |
