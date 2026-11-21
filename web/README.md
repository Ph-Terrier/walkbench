# Web simulator of walkbench: models M1 and M2

| Item | Content |
|---|---|
| Responsible author | Philippe Terrier (HE-Arc Santé, Neuchâtel) |
| Produced by | page, scripts, tests, validation reports and this README written on 2026-11-19 by the orchestrating model of the web-simulator session of SIM-ACI (Claude Opus 5.5), without a code-generation model; model M2, the generator of R and the export of R code added the same day, after the decisions of the author. The drawing, the layout and the indicative estimator are adapted from `feasibility/walker_demo.html` of the SIM-ACI project, written on 2026-08-20 by Claude Fable 5, orchestrating model of the feasibility session. On 2026-11-20, after the remarks of the author on a first trial, the same orchestrating model changed the layout of the walker and the amplitude of the preset "Beyond σmax", and added `tools/03_fall_rates.js`. Later that day, it ported the grid count of walkbench 0.7.0 and recomputed the vectors of M2. It then rewrote the status, the banner and the footer for the publication on GitHub Pages that the author chose. On 2026-11-21, it regenerated the vectors of M1 with walkbench 0.7.0, reran the tests of M1 and M2, and rewrote the status and the address of the page until the first deployment. Later that day, after the decision of the author, it changed the reading of a series from a file and added `tools/04_make_vectors_files.R` and `tests/run_tests_files.js`. After four further decisions of the author, it added a message for a file whose values are all equal and the seed of each attempt in the status line, set the static title of `index.html` to both models, and named the source file of the drawing in the footer. |
| Status | page of walkbench 0.7.0 for GitHub Pages, deployed by the workflow `.github/workflows/pages.yml`; expected address https://ph-terrier.github.io/walkbench/, to be confirmed at the first deployment; folder `web/` of the walkbench repository, excluded from the R package |
| Models reproduced | M1 and M2 of walkbench 0.7.0, whose code of the models is that of walkbench 0.6.2: M1 with the injection point `foot_strike_timing`, M2 in the configuration of section 4.10 of the analysis plan (slope 0.004 rad, r_f = 0.45, k = 5, derived impact map), within the limits of `validation/validation_m1.md` and `validation/validation_m2.md` |

## Purpose

The page simulates model M1 or model M2 of walkbench in the browser. The visitor sets the amplitude σ and the memory parameter d of an ARFIMA perturbation, or loads a series from a local file. The visitor then watches the walker, its phase portrait and its step durations, and reads indicative values of α, λS and λL during the animation.

A button starts a reference run of the transient of the analysis plan (20 steps for M1, 34 for M2) and 600 kept steps. Its outcomes follow `estimate_all()` of walkbench, with the estimation settings of the analysis plan for the model and the DFA of Terrier (2026). The visitor can change the fitting windows of λS and λL. The page then writes the R code that reruns the same run in walkbench: with the same seed, `input_arfima()` draws the same input series. After a fall, the page draws the series again with the next seed, up to 20 times. The status line numbers the attempts from 1 and shows the seed of each attempt; the R code gives the seed of the attempt that did not fall.

The page is a teaching demonstration. The published results of the SIM-ACI project come from walkbench.

## Use

The workflow `.github/workflows/pages.yml` deploys this folder on GitHub Pages. The expected address of the page is https://ph-terrier.github.io/walkbench/, to be confirmed at the first deployment. The page also works offline: open `index.html` of this folder in a browser, from the file system or from a static web server. It needs no installation, and its code sends no request to any server. The texts are in French and in English; the first language follows the language of the browser. When the operating system asks for reduced motion, the animation starts paused, and the Play button starts it.

A series loaded from a file holds one number per line, written with a decimal point, with an optional sign and exponent (`0.123`, `-1.5e-3`). The text after `#` is a comment, and blank lines are skipped. The page refuses a file with any other line, such as a number with a decimal comma, two numbers on a line or a header word, and its message gives the numbers of these lines. It also refuses a file with fewer than 120 values, and a file whose values are all equal, each with its own message. The R code exported by the page reads the same file with `scan(file, comment.char = "#")`, which reads the same numbers in the same order. In 1 of the 10,760 test values, the conversion of the text by R differs from that of the page by one unit in the last binary place (`validation/results_files.md`, check F5).

## Files

| File | Content |
|---|---|
| `index.html` | the page |
| `css/simulator.css` | style sheet, light and dark themes, system fonts |
| `js/wb_m1.js` | model M1: equations, foot strikes, fall rule, fixed point, linearization, runs |
| `js/wb_m2.js` | model M2: equations, derived impact map, foot strikes, fall rule, fixed point and continuation, linearization, runs with the bounds of each step |
| `js/wb_estimators.js` | divergence exponents (Rosenstein engine of RACING v1.0.2, nearest neighbors by a k-d tree), resampling of each step for M2, DFA of Terrier (2026), outcomes of `estimate_all()` |
| `js/wb_input.js` | generator of R (Mersenne-Twister, normal draws by inversion), ARFIMA inputs, fixed series with a circular start, reading of a local file |
| `js/wb_stream.js` | indicative divergence exponents during the animation |
| `js/wb_i18n.js` | texts in French and English |
| `js/wb_app.js` | interface, animation, reference run and export of R code |
| `tools/01_make_vectors_m1.R`, `tools/02_make_vectors_m2.R` | R scripts that compute the test vectors with walkbench |
| `tools/04_make_vectors_files.R` | R script that writes test files, reads them with `scan()` and writes the test vectors of the reading of a file |
| `tests/vectors/file_vectors.json`, `tests/run_tests_files.js` | test vectors of the reading of a file, and their comparison with the reading of the page |
| `validation/results_files.*` | results written by `tests/run_tests_files.js` |
| `tools/03_fall_rates.js` | Deno script that counts the steps before a fall in the engines of the page, at multiples of σmax; its results set the amplitude of the preset "Beyond σmax" |
| `validation/fall_rates.*` | results of `tools/03_fall_rates.js` |
| `validation/export_check/` | check of the export of R code of 2026-11-20: R code written by the page for four reference runs, rerun in walkbench, and comparison |
| `tests/vectors/m1_vectors.json`, `tests/vectors/m2_vectors.json` | test vectors written by these scripts |
| `tests/run_tests_m1.js`, `tests/run_tests_m2.js` | comparisons of the JavaScript port with the test vectors |
| `validation/results_m1.*`, `validation/results_m2.*` | results written by the comparisons |
| `validation/validation_m1.md`, `validation/validation_m2.md` | validation reports |

## Validation

The R scripts of `tools/` load walkbench from the source tree of the package with `pkgload::load_all()`. They write the values that the JavaScript port must reproduce. They need R 4.5.0 or later, pkgload, jsonlite, digest and the imports of walkbench. From the folder `web/`:

```
Rscript tools/01_make_vectors_m1.R
Rscript tools/02_make_vectors_m2.R
Rscript tools/04_make_vectors_files.R
```

The comparisons run under Deno 1.46 or later. From the folder `web/`:

```
deno run --allow-read=. --allow-write=./validation tests/run_tests_m1.js
deno run --allow-read=. --allow-write=./validation tests/run_tests_m2.js
deno run --allow-read=. --allow-write=./validation tests/run_tests_files.js
```

Each comparison writes its results to `validation/` and ends with the exit code 1 when a check fails. On the development laptop, Deno is the copy that Quarto ships with RStudio. On 2026-11-21, with the vectors of walkbench 0.7.0, all checks with a tolerance passed: 26 for M1, 24 for M2 and 6 for the reading of a file. The validation reports give the commit of each set of vectors. Some references were chosen by the author after a first run; the validation reports give the history.

The amplitude of the preset "Beyond σmax" comes from a third script, which runs the engines of the page from the fixed point with white noise:

```
deno run --allow-read=. --allow-write=./validation tools/03_fall_rates.js
```

At σmax of the analysis plan, none of the 90 runs of its calibration fell, for either model. With the engines of the page, a fall from the fixed point takes a median of several hundred steps at 1.5 σmax (M1) or 2 σmax (M2). The preset uses 3 σmax, where M1 falls after a median of about 20 steps and M2 after a median of about 40 steps (`validation/fall_rates.md`). The author chose 3 σmax on 2026-11-20, after a trial of the preset at 4 σmax, where falls came after a median of 10 steps (M1) and 17 steps (M2).

## Differences from walkbench

- Integration. walkbench integrates with `lsodar()` of deSolve (relative tolerance 1e-10). The page uses a fourth-order Runge-Kutta scheme with substeps of at most 0.001 time units, between the points of the sampling grid of walkbench (step 0.03). It locates each foot strike by bisection. The tests compare the page with walkbench run at its default tolerances and at tighter tolerances.
- Resampling of M2. Walkbench 0.7.0 counts the points of the resampling grid from the number of steps (`grid_count()`), and the page uses the same count. Walkbench 0.6.2 computed the count from the times. A rounding difference of the order of 1e-12 could then change it by one point, and λS by about 1e-4 per stride (`validation/validation_m2.md`, section 6). With walkbench 0.6.2, the R code exported by the page can therefore give λS of M2 different by about 1e-4.
- Indicative values. During the animation, the nearest neighbors are searched among the earlier samples only, and the divergence curve is an exponential mean over about 100 steps. These values serve the display. The reference run gives the values that the validation compares with walkbench.
- Not ported. The Floquet multipliers, MeanSD, the closed-loop port, the metronome, the other injection points of M1, the printed impact map of M2 and the other configurations of M2.

## Privacy and third parties

- The code of the page sends no request to any server. Its Content-Security-Policy forbids network connections, and it uses system fonts.
- The online page is hosted by GitHub Pages: GitHub receives the IP addresses of the visitors in its logs. The code of the page sends nothing.
- A local file chosen by the visitor is read in the browser and never sent.
- The choice of language is kept in the local storage of the browser when the browser allows it.
- The page contains no third-party data.

## License and citation

MIT, Copyright (c) 2026 Philippe Terrier, the license of walkbench. A citation names walkbench and its version, with the DOI of the version given by its Zenodo record. The concept DOI 10.5281/zenodo.24354488 leads to all versions.
