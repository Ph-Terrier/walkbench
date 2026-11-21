<!-- Written by tests/run_tests_files.js (2026-11-21T13:55:32.613Z); Deno 1.46.3, V8 12.9.202.5-rusty; vectors of walkbench 0.7.0, commit 754de72b70677dc048b3d7949322e79cf7704546 -->

| Test | Check | Largest difference | Tolerance | Result |
|---|---|---|---|---|
| F1 | outcome of the page for 17 files (6 accepted, 11 refused) and the lines that it reports: number of files that differ from the rule | 0 | 0 | pass |
| F2 | parseSeriesText(): same values as readSeriesText() for an accepted file, error for a refused file: number of files that differ | 0 | 0 | pass |
| F3 | files accepted by the page and refused by scan() of R, or read with another number of values | 0 | 0 | pass |
| F4 | input_fixed_series() on the values read by the page and by scan() (4 files, sd 0.00234) | 1.73e-18 | 1.00e-9 | pass |
| F5 | values read by the page whose IEEE bits differ from those of scan() (1 of 10760; the column gives the largest relative difference) | 1.50e-16 | information |  |
| F6 | files with a decimal comma accepted by the page (1 file) | 0 | 0 | pass |
| F7 | seriesProblem() with 120 values at least: number of the 9 series whose class (too few values, all values equal, valid) differs from the expected one | 0 | 0 | pass |

| File | Page | scan() of R |
|---|---|---|
| valid: 600 values, comment line, blank line, trailing comment, LF | accepted | 600 values |
| valid: 150 values, byte-order mark and CRLF | accepted | 150 values |
| valid: forms of numbers (.5, +.5, 5., exponents, leading zeros, -0), CR line ends | accepted | 10 values |
| valid: comment lines only | accepted | 0 values |
| valid: empty file | accepted | 0 values |
| conversion: 10,000 values with 6 to 21 significant digits | accepted | 10000 values |
| decimal comma | refused; reported lines: 3 (decimal comma), 5 (decimal comma) | error |
| semicolon between two numbers | refused; reported lines: 2 | error |
| several numbers on a line | refused; reported lines: 1 | 3 values |
| two columns separated by a tab | refused; reported lines: 1, 2, 3 | 6 values |
| header line | refused; reported lines: 1 | error |
| NA | refused; reported lines: 2 | 3 values, not all finite |
| Inf and an overflow | refused; reported lines: 1, 2 | 2 values, not all finite |
| hexadecimal number | refused; reported lines: 2 | 2 values |
| quoted number | refused; reported lines: 1 | error |
| no-break space after a number | refused; reported lines: 1 | error |
| thousands separator | refused; reported lines: 1 | error |
