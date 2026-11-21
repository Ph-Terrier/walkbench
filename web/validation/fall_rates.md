# Steps before a fall in the animation of the page

Written by `tools/03_fall_rates.js` (Deno 1.46.3 (V8 12.9.202.5-rusty)) on 2026-11-20. Each amplitude: 60 runs from the fixed point, white noise (d = 0) drawn by the generator of the page with the seeds 1001 to 1060, at most 2000 steps. The median time assumes the speed 1 of the animation (2.9 units of model time per second).

| Model | Multiple of σmax | σ | Runs with a fall | Median number of steps before a fall | Share of runs with a fall within 40 steps | Median time before a fall at speed 1 (s) |
|---|---|---|---|---|---|---|
| M1 | 1 | 0.0039 | 0 of 60 | more than 2000 | 0.00 | – |
| M1 | 1.5 | 0.00585 | 48 of 60 | 748 | 0.02 | 1001 |
| M1 | 2 | 0.0078 | 60 of 60 | 91 | 0.27 | 121 |
| M1 | 3 | 0.0117 | 60 of 60 | 20 | 0.80 | 26 |
| M1 | 4 | 0.0156 | 60 of 60 | 10 | 0.97 | 13 |
| M1 | 5 | 0.0195 | 60 of 60 | 8 | 1.00 | 11 |
| M2 | 1 | 0.0003 | 0 of 60 | more than 2000 | 0.00 | – |
| M2 | 1.5 | 0.00045 | 0 of 60 | more than 2000 | 0.00 | – |
| M2 | 2 | 0.0006 | 57 of 60 | 648 | 0.00 | 300 |
| M2 | 3 | 0.0009 | 60 of 60 | 40 | 0.55 | 18 |
| M2 | 4 | 0.0012 | 60 of 60 | 17 | 0.98 | 8 |
| M2 | 5 | 0.0015 | 60 of 60 | 11 | 1.00 | 5 |

A median of more than 2000 steps means that fewer than half of the runs fell within 2000 steps; its time is then not given.
