# walkbench 0.6.2 (first public release, 2026-11-07)

User guide in the public repository, 2026-11-18. This addition applies the decision of the author of 2026-11-14: the user guide is published with version 0.6.2, in `docs/` of the repository and outside the build of the package. The author decided on 2026-11-18 that the example scripts of the guide are published with it. The coordination session (Claude Fable 5.2) passed on the tasks on 2026-11-18. No change of code; the version stays 0.6.2.

- `docs/walkbench_user_guide.pdf`: the user guide.
- `docs/scripts/`: the 18 example scripts that produced the code blocks, outputs, numbers and figures of the guide, and the table of the functions that two of them read. The SP1.1 session adapted the paths of these scripts to the public repository.
- `.Rbuildignore`: the folder `docs/` is excluded from the build of the package. `.gitignore`: the outputs of the example scripts (`docs/generated/`) are not tracked.
- `README.md`: section "User guide". The section "License" states the license of the guide. Its text and figures are under CC BY 4.0, and the code of its examples is under MIT. A sentence of the section "Installation" states the maximal length of the path of the R library under Windows. Three sentences on the pinned code and the tests no longer name a sub-project of the author.
- `.zenodo.json` and `CITATION.cff`: one sentence on the guide and its example scripts at the end of the description. The notes of `.zenodo.json` state the two licenses. The field `license` of both files stays MIT, the license of the code.
- `.zenodo.json`, `CITATION.cff`, `DESCRIPTION` and `README.md`: the registration of the analysis plan on OSF Registries is cited by its DOI, 10.17605/OSF.IO/M74TK, since the registration became public on 2026-11-18. The code, its comments, the help pages and the tests keep the address osf.io/m74tk, which stays valid.
- Heading of version 0.6.2: "first public release" in place of "development".
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

Search of the fixed points, 2026-11-13. The SP1.1 session sorted the observations of the session that writes the user guide of walkbench (Claude Fable 5.2). This addition follows that triage and the answer of the author of 2026-11-13, passed on by the coordination session (Claude Fable 5.2). No change of code; the version stays 0.6.2.

- `README.md`: a paragraph after the description of the models. The search of the fixed point of M1 tests neither convergence nor stability; the search of M2 tests convergence only. The field `eig` of `fixed_point()` serves as the check of stability. The paragraph names the slopes and the configuration that the tests check and that the campaigns of the first article used.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

Minimum version of R, 2026-11-13. This correction follows a finding of the session that writes the user guide of walkbench (Claude Fable 5.2). The coordination session (Claude Fable 5.2) verified the finding and passed it on after the answer of the author of 2026-11-13. No change of code; the version stays 0.6.2.

- `DESCRIPTION`: the field `Depends` declares `R (>= 4.5.0)` in place of `R (>= 4.1)`. The section "Installation" of `README.md` states the same requirement and its reason.
- Reason. Three parts of the package compute an MD5 sum with the argument `bytes` of `tools::md5sum()`: `load_racing()`, the pinned walker file and a test of the pinned code. R 4.5.0 introduced this argument (file `doc/NEWS` of R, "CHANGES IN R 4.5.0", "NEW FEATURES"). Before this correction, the package could be installed under an older version of R. `load_racing()` and the first use of M1 then stopped with an error.
- The SP1.1 session searched the news of R 4.1.1 to 4.5.1 for the functions, arguments and syntax that the code of `R/`, `tests/` and `inst/pinned/` uses. It found no other feature newer than R 4.1. The package defines its own operator `%||%`, which base R provides since version 4.4.0.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

Metadata for the public repository of walkbench, 2026-11-12 (evening). This change follows the task of the coordination session (Claude Fable 5.2), after decisions of the author of the same evening: one initial commit with the content of version 0.6.2, and the DOI of each version created by Zenodo at its release on GitHub. No change of code; the version stays 0.6.2.

- `README.md`: the status row gives the address of the public repository and the archiving of each release on Zenodo. New sections on installation, on the files of the SIM-ACI project named in the code, and on citation.
- `DESCRIPTION`: fields `URL` and `BugReports`. `man/walkbench-package.Rd` adds the links that roxygen2 derives from these fields.
- New files `CITATION.cff`, `.zenodo.json` and `.gitignore`. `.Rbuildignore` keeps them out of the source package.
- `README.md`, later the same evening: the table of the main functions adds `check_output_folder()`, the 60th exported function. The section "Example" shows how to reach the fixed point of M2 in the configuration of the first article by continuation in r_f and k, since the default initial guess of `model_m2()` does not reach it. These corrections follow observations of the session that writes the user guide of walkbench (Claude Fable 5.2), passed on by the coordination session after the answer of the author of 2026-11-12.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

Metadata changes of 2026-11-12, before the publication of the package, on the request of the session that builds the reproduction archive of the first article (go of the author given to the SP1.1 session on 2026-11-12 at about 00:19). No change of code; the version stays 0.6.2.

- `DESCRIPTION` and `man/walkbench-package.Rd`: the address of the maintainer is the institutional address of the title page of the first article.
- `README.md`: the status line no longer states when the repository becomes public.
- `inst/pinned/MANIFEST.csv`: the column `origin_repository` no longer names the private repository of the project; the paths, commits and SHA-256 sums are unchanged.
- `DESCRIPTION`: the field `Description` describes the content of version 0.6.2: the interface, the models M1 and M2 with their references, RACING v1.0.2 with the DOI of that version, the campaign rules and the analytic predictor. It no longer describes version 0.1.0. This change follows the task of the coordination session (Claude Fable 5.2), after the answer of the author in that session on 2026-11-12. The help page `man/walkbench-package.Rd` has its own text, which already describes version 0.6.2; it is unchanged.
- `README.md`: the passages on the specification, the content of the version, loading, pinned code and tests describe version 0.6.2 for a reader of the published package, and the reference list adds Terrier (2026). This change follows the task of the coordination session (Claude Fable 5.2), after the answer of the author in that session on 2026-11-12.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

Fixes of the review of the code of campaigns 2 and 3 of M1 by a verification sub-agent (Claude Fable 5.1), on the state of walkbench 0.6.1 frozen on 2026-11-07 at 08:11. The author approved the triage on 2026-11-07 at 08:41 (register `SIMACI/docs/author_decisions.md`; `SIMACI/docs/reviews/2026-11-07_triage_campaigns_m1.md`).

- F1, numerical failures (option B, decided by the author on 2026-11-07 at 08:17). In `run_trial()`, an attempt counts as a numerical failure when it ends as a fall after at least one warning of the solver. This is the definition of section 4 of update 1 of the registration osf.io/m74tk. A flag of the attempt is set in the warning handler, with the criterion of the errors (`from_integrator()`). The message of the failure joins the warnings of the attempt. An attempt that completes keeps its outcomes. The pinned walker file is unchanged.
- F4, fixed columns. For a run that fell, `estimate_all()` returns the outcome columns, empty (internal function `outcome_columns()`). The summary row of a trial without outcomes has the same columns. The columns of `runs.csv` thus stay the same when no simulation of a campaign has outcomes.
- F2. `check_output_folder()` stops when an output folder exists and is not empty, unless `overwrite = TRUE`, and when the folder lies inside a forbidden folder. The campaign scripts call it before they write any file.
- F7 and F10. `write_campaign_log()` records `rng_kind` (`RNGkind()`), the versions of pkgload and parallel, and the status of the whole repository (`whole_repository`).
- F8, accepted risk. The help of `run_trial()` describes the R error of the pinned `integrate_step()` near the time limit of 15 time units, which stops a campaign.
- Behavior change: the summary row of a trial without outcomes has more columns, all empty. No archived output of SIM-ACI used `run_trial()` of walkbench 0.6.0 or 0.6.1.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model, on the task of the coordination session (Claude Opus 5.5).

# walkbench 0.6.1 (development, 2026-11-07)

Classification of the errors of `run_trial()`, under section 4 of update 1 of the registration osf.io/m74tk, unchanged:

- Only an error of the integrator counts as a numerical failure. An error counts as an error of the integrator when the compiled solver of deSolve raised it (internal function `from_integrator()`). The frame of origin is the innermost frame whose call is the call of the condition. When the condition is signaled, that frame must run a function of the namespace of deSolve, and no call to `stop()` may lie between that frame and the handler.
- Any other error of `simulate_run()` stops the trial, and so the campaign, with its original message and call. Such errors include the errors of the model code and of a controller, of the right-hand side and of the root function. They also include an argument check of the R code of deSolve, and a function that is not found.
- The criterion uses neither the class of the condition nor its message. deSolve signals simple conditions, and its messages change with its version. The call alone does not serve either: when deSolve is not attached on a parallel R process, the error "could not find function "lsodar"" has the call `lsodar(...)`, but no frame runs `lsodar()`. In walkbench 0.6.0 this error counted as a numerical failure at every attempt (SIMACI/wp2_injection/README.md, scripts of the campaigns of M1).
- With deSolve 1.42, the compiled solver raises an error when it rejects its input before the first step (`istate = -3`). It reports its other error returns (`istate` -1, -2 and -4 to -7) as warnings and returns a truncated output. The integration loops then find no foot strike and record a fall, in M1 (pinned walker file) and in M2. `run_trial()` returns these warnings in `integrator_warnings`, with the number of the attempt, and does not change the outcome of the attempt.
- Behavior change: a campaign now stops on an error of the model code, of a controller or of an argument check of deSolve. In walkbench 0.6.0 such an error counted as a numerical failure. No archived run of SIM-ACI used the classification of walkbench 0.6.0: the calibration scripts of SIMACI/wp2_injection have their own error handler, and the prediction scripts do not simulate.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model. Task of the coordination session (Claude Opus 5.5), after the answer of the author of 2026-11-07 at 07:31 (register `SIMACI/docs/author_decisions.md`).

# walkbench 0.6.0 (development, 2026-11-06)

Update 1 of the registration osf.io/m74tk (text `SIMACI/docs/osf/SIMACI_update_1.md`, commit 0fbc635):

- Predictor of M2 (rules E1 to E5): `predict_spinal_levels()` takes the maps to the coordinates of the estimation as an argument (`maps`) and the step between separations (`separation_step`, 1 for odd and even separations). Its argument `terms = "durations"` gives the descriptive comparison by the cumulated step durations alone. Without `maps`, the maps of the linearization are used for M1 only, since for M2 they are the continuous leg coordinates of a sensitivity analysis (finding 6 of the review of the update). The defaults return the values of 07b.
- `spinal_n_past()` applies rule E4: the smallest n with 0.923^n <= 1e-4 and |J^n b| <= 1e-4 |b|. In M2 it gives n = 124; the first condition alone gives 115, and at n = 115 the norm ratio is 2.0e-4. With 124 past inputs, the node of 1,000 steps needs the lag 1,143 of the autocovariance.
- `spinal_settings_m2()` gives the maps, the separations, the levels K = 8 to 20 and the number of past inputs. Its grid has the nodes 2 to 20, 30, 50, 80, 120, 200, 300, 400 and 500 steps. Records of 1,200 steps add the nodes 700, 900 and 1,000 (section 3 of the update). `spinal_lambda_l_lsq()` gives twice the least-squares slope of the levels, lambda_L per stride (rule E3).
- Option `initial_rank_tol` of `predict_spinal_levels()` (default `NULL`, as in 07b): the epochs are matched on the subspace that the input reaches, with the density at zero of the initial difference taken on that subspace. In M2 the step map J has rank 2, and the input reaches three of the four directions of the state after contact; the fourth, mostly phi_dot, is never excited. The covariance of the initial difference is then singular, and the matching of 07b is not defined. `spinal_settings_m2()` returns `initial_rank_tol = 1e-8`, the rule of update 2 of the registration (`SIMACI/docs/osf/SIMACI_update_2.md`, commit f5f3be8, approved on OSF on 2026-11-07); the predictions of M1 are not computed again.
- Numerical failures (section 4 of the update): `run_trial()` counts an error of the integrator as a numerical failure, distinct from a fall. A generated input is redrawn within the 21 attempts; a fixed input ends the trial as after a fall. The summary rows give `n_falls` and `n_numerical_failures`. `summarize_cells()` reports the failures, their rate and the combined rate, and flags a cell (`flag`) when the fall rate or the combined rate exceeds 20%.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

# walkbench 0.5.0 (development, 2026-11-06)

Analytic predictor of section 5.3 of the SIM-ACI analysis plan (frozen at commit ba72afe):

- `predict_spinal_levels()` gives the expected divergence levels of the candidate form of SPINAL from the linearization of a model and the autocovariance of its input. It repeats `predict_levels()` of `07b_walker_prediction.R` of the repository "time series R" at commit ced76be, pinned unchanged in `inst/pinned/time_series_R/` with its SHA-256 sum. The input autocovariance is an argument, and the size of the reduced state is read from the linearization. `spinal_lambda_l_m1()` gives the predicted lambda_L of M1, [y(4 steps) - y(3 steps)] / T.
- With an FI(d) input centered over 620 values, 600 steps and the linearization of M1, the values equal those of the pinned 07b to 1e-12 (`tests/testthat/test-predictor.R`), and the printed values of 07b are reproduced.
- Inputs: `fi_autocorrelation()`, `ar1_autocorrelation()`, `centered_autocovariance()` (the approximation of 07b for a series centered and rescaled over a window), `sample_autocovariance()`.
- AR family (section 4.9): `window_slope_half_log_v()` and `match_fi_to_ar1()`, the slope of 0.5 ln v(K) and the matched FI level of `10_ar1_against_fi.R` of SPINAL; the matched levels over 2 to 4 cycles of SPINAL are reproduced.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

# walkbench 0.4.0 (development, 2026-11-06)

Rules of the SIM-ACI analysis plan frozen at commit ba72afe and registered on OSF (osf.io/m74tk), for the campaigns:

- Seeds (section 4.3): `campaign_table()` and `campaign_master()` give the master seeds 100,000,000 + 10,000,000 j of the seven campaigns. `seed_campaign()` now requires cells and replicates numbered from 1 under these masters, and it still allows 21 attempts (0 to 20). `number_cells()` numbers the cells from 1 with the first factor (d) varying fastest. `bootstrap_seed()` gives 20261106. `check_seed_plan()` checks that the master seeds of the calibration runs stay below 100,000,000 and apart from the campaigns, the surrogates and the earlier runs.
- Transient under a controller (section 4.3): `transient_cycles_loop()` takes the largest eigenvalue modulus of the linearized closed loop at the gain of the cell. At a gain of zero it uses the open loop, since the loop of H3b then keeps the eigenvalue 1 of the uncorrected asynchrony. `transient_from_radius()` applies the rule to a given modulus.
- Rescaling (section 4.3): `input_arfima(n_transient = )` centers and rescales a realization on its values after the transient. The default `n_transient = 0` keeps the rescaling of the whole series of the feasibility study and of the calibration runs of M2.
- Real stride series (section 4.8): `input_fixed_series()` rescales the window and fills the transient with its last values (circular start, which replaces choice O2 of the interface specification). `seed_surrogate()` and `input_surrogate()` give the permutations with the seeds 70,000,000 + 1,000 s + i. `draw_fixed()` draws only the initial jitter for an input that is not redrawn.
- DFA (section 5.2): `dfa_settings()`; the default is the configuration of Terrier (2026, Physica A 699:131842), DFA-1 with up to 30 box sizes from 16 values to one third of the series and no alpha below 96 values. It equals `compute_dfa_alpha()` of the Zenodo archive of that article to 1e-15 on the test series. The DFA of the input uses its kept part. `dfa_settings("feasibility")` keeps the frozen DFA of the feasibility study for the replay. `estimate_all()` adds `alpha_stride_duration`.
- Falls (section 4.4): `bind_trials()` and `summarize_cells()` give per cell the attempts, the falls, the fall rate (falls / attempts), the flag above 20% and the inclusion when at least 80% of the planned replicates have outcomes.
- Behavior change: `run_trial()` and `estimate_all()` use the DFA of the plan by default. `SIMACI/wp1_replication/01_replay_feasibility.R` passes `dfa_settings("feasibility")`.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

# walkbench 0.3.0 (development, 2026-11-06)

- `transient_cycles(model)`: number of transient cycles under the rule that the author chose on 2026-11-06 at 13:55 (SIM-ACI analysis plan, draft 0.5, sections 4.3 and 4.8). It is the smallest n such that lambda^(2n) <= 0.005, with a minimum of 20 cycles, where lambda is the largest modulus of the eigenvalues of the cycle map at the fixed point. M1 keeps 20 cycles; M2 with r_f = 0.45 and k = 5 takes 34 cycles.
- `run_trial()`: the default number of transient cycles is `transient_cycles(model)`. It equals the former default of 20 for M1, so the replay of the feasibility study is unchanged.
- The adapter test checks the eigenvalues of every model and the rule of the transient.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

# walkbench 0.2.0 (development, 2026-11-06)

- Model M2: the primary estimation settings are those of table 5.1 of the SIM-ACI analysis plan, draft 0.4, approved by the author on 2026-11-06 at 12:26. The state space is the four state variables of the model (theta, theta_dot, phi, phi_dot), referred to the stance leg. Each grid point of the global resampling (100 samples per stride on average) is interpolated inside its own step, so that the jumps at foot strike stay jumps. Neighbors are excluded within half a stride (50 samples). The curve is the mean of the ln distances over a shrinking set of pairs, and the slopes are least-squares fits over 0 to 0.5 and 4 to 10 strides. Bruijn et al. (2012) state the state space, the resampling and the windows; the other settings follow `lds_calc.m`, the public code of S. M. Bruijn. The estimates equal those of the designated cell of `SIMACI/wp1_replication/06_m2_estimation_sensitivity.R`.
- `settings_m2()` is exported. `settings_m2("continuous")` returns the former primary settings (continuous leg coordinates, global resampling, neighbor exclusion of one stride), which the replication runs used and the plan keeps as a sensitivity analysis.
- `estimate_divergence()`: resampling `"global_by_cycle"` and estimation in the state variables of the model (`settings$coordinates = "model"`, with the maps of `settings$model_coordinates`).
- Behavior change: `model_m2()$estimation_settings` now gives the new settings. Scripts that need the former settings call `settings_m2("continuous")`.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

# walkbench 0.1.1 (development, 2026-11-06)

- Model M2: option `impact` of `model_m2()`. The value "printed" applies the impact map as printed by Hobbelen and Wisse (2007, p. 1223), whose denominators are evaluated before the strike; the derived map stays the default. The two maps are equal on level ground and at r_f = 0. The option serves a diagnostic of the replication of M2 (`SIMACI/wp1_replication/07_m2_printed_impact.R`), approved by the author on 2026-11-06.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.

# walkbench 0.1.0 (development, 2026-11-05)

- Common interface of the SIM-ACI testbed, specification 0.2 (sub-project SP1.1): perturbation port, closed-loop port with metronome and the controllers of variants H3a and H3b, linearization at the fixed point, standardized outputs, common estimators, run driver with the seed rule and the campaign log.
- Model M1, the simplest walker with foot-strike perturbations, from the pinned walker file of the feasibility study.
- Model M2, the walker with arced feet and hip spring (Hobbelen and Wisse 2007; Bruijn et al. 2012), with equations derived and checked against the printed ones; the printed swing-leg equation has a sign error in its third term.
- Pinned copies: `00_functions_walker.R` of the feasibility study and the Rosenstein engine of RACING v1.0.2, with a manifest of SHA-256 sums checked at load time.
- Written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model.
