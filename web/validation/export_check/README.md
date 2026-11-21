# Check of the export of R code (2026-11-20)

| Item | Content |
|---|---|
| Responsible author | Philippe Terrier (HE-Arc Santé, Neuchâtel) |
| Produced by | check run twice on 2026-11-20 by the orchestrating model of the web-simulator session of SIM-ACI (Claude Opus 5.5), which wrote `page_values.js`, `check_exports.R` and this file. The page wrote the four R files in the browser pane of the Claude desktop app. The two scripts write `page_values.json`, `serie_lcg.txt`, `walkbench_values.csv` and `comparison.md`. |
| Reference | first run: walkbench 0.6.2 loaded from the source tree of the SIM-ACI repository (`testbed/`), under R 4.5.1 (Windows 11). Second run, the same day: walkbench 0.7.0 from the same source tree, a local version not yet released, with uncommitted changes; `comparison.md` gives the SHA-256 of the files that ran. Third run, on 2026-11-21, by the same model: walkbench 0.7.0 at commit 754de72, after the change of the reading of a file (section "Rerun of 2026-11-21"). |

## Procedure

1. The page was served from a local web server and opened in the browser pane of the Claude desktop app. The orchestrating model ran four reference runs there, with the windows of the analysis plan:
   - M1 with an ARFIMA input: seed 7, d = 0.3, σ = 0.00234 rad;
   - M2 with an ARFIMA input: seed 1, d = 0, σ = 0.00018 leg lengths;
   - M1 and M2 with the 600 integers of `serie_lcg.txt`, loaded through the file input of the page, at the same amplitudes.
2. The R code that the page wrote for each run is in `m1_arfima.R`, `m2_arfima.R`, `m1_file.R` and `m2_file.R`. The line `library(walkbench)` became a comment, since walkbench is not installed on the laptop; `check_exports.R` loads the package from its source tree.
3. `page_values.js` reruns the four runs with the files of the page under Deno and writes `page_values.json`. Its values agree with those that the browser pane showed to 2 × 10⁻¹¹, with the same numbers of pairs.
4. `check_exports.R` writes `serie_lcg.txt`, sources the four R files and writes `walkbench_values.csv` and `comparison.md`.

From the folder `web/`, then from this folder:

```
deno run --allow-read=. --allow-write=./validation validation/export_check/page_values.js
Rscript check_exports.R
```

## Rerun of 2026-11-21

Since 2026-11-21, the page accepts one number per line written with a decimal point and refuses a file with any other line (`../../README.md`, section "Use"). `serie_lcg.txt` follows this format. `page_values.js` now reads it with the new reading, and `check_exports.R` was rerun with walkbench 0.7.0 at commit 754de72. `page_values.json` and the comparison table of `comparison.md` are the same as in the second run; only the dates and the run times in `walkbench_values.csv` changed.

The R code of the page now holds the comment `# one number per line, with a decimal point; the text after # is a comment` before the call of `scan()`. The files `m1_file.R` and `m2_file.R` keep the comment that the page wrote on 2026-11-20; their lines of code are the same.

## Results with walkbench 0.7.0 (second run, 2026-11-20)

Walkbench 0.7.0 counts the points of the resampling grid from the number of steps (`grid_count()`), and the page follows it. The full table is in `comparison.md`.

- The page and walkbench received the same input series: the α of the inputs agree to 8 × 10⁻¹⁶.
- All four runs agree. λS and λL agree to 8.4 × 10⁻⁶ or better, the α of the step durations to 6 × 10⁻⁹, and the numbers of pairs are the same.
- M2 with the ARFIMA input of seed 1: λS now agrees to 4.8 × 10⁻⁷ and λL to 6 × 10⁻⁹, with 28,117 pairs in both codes. The page values did not change; walkbench now uses 30,001 grid points, as the page does.
- The three other runs give the same values as in the first run.

## Results with walkbench 0.6.2 (first run, 2026-11-20)

- The page and walkbench received the same input series: the α of the inputs agree to 8 × 10⁻¹⁶.
- M1: λS and λL agree to 8.4 × 10⁻⁶ or better, and the α of the step durations to 7 × 10⁻¹⁰. The numbers of pairs are the same.
- M2 with the series of the file: λS agrees to 3.7 × 10⁻⁷ and λL to 6 × 10⁻¹⁰, with the same number of pairs.
- M2 with the ARFIMA input of seed 1: λS differs by 1.2 × 10⁻⁴ per stride, and the page counts 28,117 pairs against 28,114.

A diagnostic of the session traced this last difference to the grid of `resample_states()` (`validation_m2.md`, section 6); its script lies outside the repository. The argument of `floor()` is 30,000.000000000007 in the page and 29,999.999999999993 in walkbench. The page thus uses 30,001 grid points and walkbench 30,000. With 30,000 points, the page gives λS = 2.982660 per stride, λL = 0.0270935 and 28,114 pairs, within 4.8 × 10⁻⁷ of walkbench. The author decided on 2026-11-19 to correct this formula in walkbench 0.7.0; the second run above checks that correction. The values of this first run are no longer in `comparison.md`, which the second run rewrote.
