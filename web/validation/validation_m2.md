# Validation of the JavaScript port of model M2

| Item | Content |
|---|---|
| Responsible author | Philippe Terrier (HE-Arc Santé, Neuchâtel) |
| Produced by | report written on 2026-11-19 by the orchestrating model of the web-simulator session of SIM-ACI (Claude Opus 5.5). The numbers come from `validation/results_m2.md` and `validation/results_m2.json`, written by `tests/run_tests_m2.js`, and from `tests/vectors/m2_vectors.json`, written by `tools/02_make_vectors_m2.R`. Two diagnostics in sections 5 and 6 were run from scripts of the session outside the repository; their results are restated here. The same model revised the report on 2026-11-20 for walkbench 0.7.0 (section 6), and on 2026-11-21 for the line "Reference" and the rerun of the tests. |
| Reference | since 2026-11-20: walkbench 0.7.0 loaded from the source tree of the SIM-ACI repository, under R 4.5.1 (Windows 11) with deSolve 1.42, RANN 2.6.2 and MASS 7.3-65. The vectors were computed on 2026-11-20 from commit dee218f with the changes of walkbench 0.7.0 not yet committed. They record the SHA-256 of the ten files of the package that they use. On 2026-11-21, these sums equal those of the files at commit 754de72, and at commit 136093c, which committed these changes. On 2026-11-19: walkbench 0.6.2 at commit 05387fd. |
| Configuration | section 4.10 of the analysis plan: slope 0.004 rad, foot radius r_f = 0.45, hip stiffness k = 5; derived impact map of walkbench |
| JavaScript run time | Deno 1.46.3 (V8 12.9) |
| State | runs of 2026-11-20 and 2026-11-21 with walkbench 0.7.0: 32 checks, 24 with a tolerance, all passed, and 8 for information; the two runs give the same values. In the first run, on 2026-11-19, 2 checks failed (T5e, T10); sections 5 and 6 give the causes and the decisions of the author. |

## 1. Scope

The page computes model M2 with `js/wb_m2.js` and its outcomes with `js/wb_estimators.js`. The validation follows that of M1 (`validation/validation_m1.md`). It adds the steps proper to M2: the fixed point reached by continuation, the resampling of each step, the state variables of the model and the least-squares slopes.

## 2. Test vectors

`tools/02_make_vectors_m2.R` computed, with walkbench:

- the fixed point of the configuration, reached by the continuation of the README of walkbench, and the linearization of `linearize()`; the fixed point of the starting configuration (r_f = 0, k = 0);
- the tight reference: the fixed point and a run of 634 steps with the samples of six steps, computed by the functions of M2 at the lsodar tolerances rtol 1e-13 and atol 1e-15;
- a run of 634 steps (34 transient and 600 kept) with its step events, samples and outcomes of `estimate_all()`; seed 1, d = 0.3, σ = 1.8 × 10⁻⁴ leg lengths, the reference amplitude of M2;
- a run of 144 steps with its trajectory, the bounds of its steps and its resampled state variables;
- the divergence exponents of that run, under the settings of the plan and under the windows 0 to 1 and 3 to 8 strides;
- a run that falls (seed 2001, d = 0.45, σ = 4.5 × 10⁻⁴), at step 289.

## 3. Tolerances

The orchestrating model applied to M2 the tolerances and the references that the author chose for M1 on 2026-11-19 (`validation/validation_m1.md`, section 3). After the first run of the tests of M2, the author chose the same day two references, among options put by the orchestrating model:

- the sampled states are tested against the tight reference, at 1e-7 (section 5);
- the end-to-end exponents λS and λL are tested with the number of grid points of walkbench, at 1e-4 (section 6).

From walkbench 0.7.0, the page and walkbench count the grid points in the same way (section 6). Since 2026-11-20, T10 therefore uses the grid of the page, and T10e checks that the two counts are equal, with a tolerance of 0.

## 4. Results

The full table is in `validation/results_m2.md`. The main results:

- Fixed point. Against the tight reference, the fixed point and the period agree to 1.5 × 10⁻¹² (T1). The continuation of the README of walkbench, run in JavaScript from the default guess, reaches the same point to 1.6 × 10⁻¹² (T1c). The transient of the analysis plan is 34 steps in both codes (T2c).
- Linearization. The moduli of the eigenvalues agree to 6.7 × 10⁻⁸ (T2); the largest modulus is 0.9229.
- Runs. Against the tight reference, the step durations agree to 6.9 × 10⁻¹² and their cumulative sums to 5.3 × 10⁻¹⁰ (T5, T5f). Against walkbench, the post-strike states agree to 1.9 × 10⁻¹⁰ (T5b). The runs have the same 28,397 samples, the same legs and no fall. The run with a fall stops at step 289 in both codes (T6).
- Sampled states. Against the tight reference, the sampled states of six steps agree to 1.05 × 10⁻⁹ (T5e). Against walkbench at its default tolerances, they differ by 8.2 × 10⁻⁷ (T5h, information).
- Estimators on the values of R. The resampled state variables are identical (T7r, 5,501 rows). The divergence curves agree to 8.9 × 10⁻¹⁶ and λS and λL to 2.4 × 10⁻¹⁵ (T7, T8). All neighbors are the same, and so are the numbers of pairs, the horizons (1,000 samples) and the Theiler windows (50 samples).
- End to end. The page and walkbench 0.7.0 both use 30,001 grid points (T10e). λS is 2.485344 per stride in JavaScript and in walkbench, and λL is 0.074031 in both; the largest difference is 2.6 × 10⁻⁷ (T10). The numbers of complete pairs are the same, 28,040 (T10d). The alpha values agree to 3.8 × 10⁻⁹ (T10b), and the statistics of the step durations to 6.6 × 10⁻¹⁰ (T10c).

## 5. Sampled states: cause of the first failure of T5e

In the first run, the reference of T5e was walkbench at its default tolerances, and T5e failed at 8.2 × 10⁻⁷. The sampled states of walkbench at its default tolerances differ from those of the tight reference by 8.2 × 10⁻⁷, and those of the page by 1.05 × 10⁻⁹. As for the fixed point of M1, the difference comes from the integration error of walkbench at its default tolerances (rtol 1e-10, atol 1e-12). The author then chose the tight reference for the sampled states, in M2 and in M1.

## 6. End-to-end exponents: cause of the first failure of T10

The settings of the plan for M2 resample the run on a uniform grid of `floor((t1 − t0) / mean_unit × n_per) + 1` points (function `resample_states()` of walkbench). Here, t1 − t0 is the duration of the kept steps, mean_unit is twice the mean step duration, and n_per = 100. For a run of 600 kept steps, the argument of `floor()` equals 30,000 in exact arithmetic. Rounding errors of the order of 1e-12 decide whether `floor()` returns 29,999 or 30,000, so the grid has 30,000 or 30,001 points.

In the run of the tests, the argument is 29,999.999999999993 for the page and 30,000.000000000000 for walkbench (row T10e of the run of 2026-11-19, an information row). The page uses 30,000 grid points and walkbench 30,001. In the first run, T10 used the grid of the page and failed at 1.16 × 10⁻⁴. With the 30,001 points of walkbench, the page gives λS = 2.485344 and λL = 0.074031, within 2.6 × 10⁻⁷ of walkbench, with the same 28,040 complete pairs.

Two diagnostics of the session support this cause:

- The same run computed by walkbench at the tight tolerances gave λS = 2.4853440 and λL = 0.0740309, with the same number of pairs. The integration error of walkbench therefore does not explain the difference.
- The estimators of the page were applied to the whole trajectory of walkbench: samples, bounds and events of the 634 steps. They gave λS and λL equal to those of walkbench to 1e-15, with the same 30,001 grid points.

This sensitivity belongs to the formula of walkbench. Since every run of the analysis plan keeps a whole number of strides, the argument of `floor()` is always close to an integer. The number of grid points can then depend on the platform or on rounding differences of the order of 1e-12, and λS can change by about 1e-4 per stride (5 × 10⁻⁵ in relative terms). The export check of section 7 showed a difference of the same size.

Decisions of the author of 2026-11-19. T10 now uses the number of grid points of walkbench; the result with the grid of the page stays as T10f, for information. The formula is to be corrected in walkbench 0.7.0, so that the grid always includes the end of the run, and the page will then follow the new formula. Until that version, the page keeps the formula of walkbench 0.6.2.

Correction in walkbench 0.7.0 (2026-11-20). The SP1.1 session of SIM-ACI (Claude Opus 5.5) added the function `grid_count()` to `R/estimators.R` of the local walkbench 0.7.0. The count is now `floor(n_cycles × n_per / cycles_per_unit) + 1`, computed from the number of steps: 30,001 points for 600 kept steps. The orchestrating model of the web-simulator session ported the same count to `js/wb_estimators.js`, and removed the override of the count that T10 used. The vectors of M2 were recomputed with walkbench 0.7.0. Their outcomes are identical to those of 0.6.2, since walkbench already used 30,001 points in this run. All checks with a tolerance pass. T10 passes with the grid of the page, at 2.6 × 10⁻⁷. T10e passes, with 30,001 points in both codes. The information rows T10e and T10f of 2026-11-19 are removed.

## 7. Export of R code

On 2026-11-19, the page ran a reference run of M2 with the seed 1 (d = 0, σ = 1.8 × 10⁻⁴). The R code that the page wrote was run with walkbench loaded from its source tree. walkbench gave λS = 2.98266, λL = 0.0270936 and α of the input 0.49008, with 28,114 complete pairs. The page showed λS = 2.9828, λL = 0.0271 and α of the input 0.490, with 28,117 pairs. The input series are the same. λS differs by about 1e-4, the size of the effect of section 6.

On 2026-11-20, the check of `validation/export_check/` repeated this run, with the same values. A diagnostic of the session confirmed the cause: the page used 30,001 grid points and walkbench 30,000. With 30,000 points, the page agreed with walkbench to 4.8 × 10⁻⁷, with 28,114 pairs. The same check ran M2 with a series read from a file: λS and λL agreed to 3.7 × 10⁻⁷, with the same number of pairs.

Later the same day, the check ran again with walkbench 0.7.0. For the run with the seed 1, λS of the page and of walkbench agreed to 4.8 × 10⁻⁷ and λL to 6.1 × 10⁻⁹, with 28,117 pairs in both codes. The run with the file series gave the same values as before (`validation/export_check/comparison.md`).

## 8. Limits of the validation

- The printed impact map of walkbench (option `impact = "printed"`) is not ported.
- The indicative values of the animation (`js/wb_stream.js`) use the continuous coordinates and the mean of local slopes, with the windows of the plan converted to steps. They are not compared with walkbench.
- The animation of M2 was checked on 2026-11-19 in the browser pane of the Claude desktop app, with a local web server, through a test hook of the page (`WB.app.step()`), since the pane was hidden. The drawing of the arced feet was checked on screenshots. The hook advanced the walker 14 steps in 20 time units, with an advance of 0.34 leg lengths per step, the step length of the fixed point.
- On 2026-11-20, after a first trial of the author in Chrome, the orchestrating model changed the layout of the page and the amplitude of the preset "Beyond σmax" (`README.md`). It left the JavaScript files of the models, of the inputs and of the estimators unchanged, and it did not rerun the checks of this report. Later the same day, it changed the grid count of the estimators for walkbench 0.7.0 and reran the checks (section 6).
