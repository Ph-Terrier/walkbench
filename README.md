# walkbench: testbed of walking models for the divergence exponents of gait

| Item | Content |
|---|---|
| Responsible author | Philippe Terrier (HE-Arc Santé, Neuchâtel) |
| Produced by | code, tests and this README written on 2026-11-05 by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model; pinned files copied unchanged (section "Pinned code"); passages on the specification, the content of version 0.6.2, loading, pinned code and tests revised on 2026-11-12 by the same session; status row and sections on installation, files of the SIM-ACI project and citation revised or added on 2026-11-12 in the evening by the same session, for the public repository; table of the main functions and fixed point of M2 in the section "Example" revised on 2026-11-12 in the evening by the same session; requirement on the version of R in the section "Installation" corrected on 2026-11-13 by the same session; paragraph on the search of the fixed points added on 2026-11-13 by the same session; section "User guide", license of the guide and sentence on Windows in the section "Installation" added, and three sentences on the pinned code and the tests reworded, on 2026-11-18 by the same session, for the public repository; rows "Version" and "Status", sections on the installation, the user guide, the web simulator, the tests, the producers and the citation revised or added on 2026-11-20 by the same session, for version 0.7.0, with the names of the predictor, the concept DOI, the preprint of the first article and the publication of `web/`; row "Status", the versions of the first article and the version of the validation of `web/` revised on 2026-11-21 by the same session, for the release of version 0.7.0; section "User guide", the producers of the guide and the address of the page revised on 2026-11-21 by the same session, for the guide of version 0.7.0; address of the page in the section "Web simulator" confirmed on 2026-11-21 by the same session, after the first deployment |
| Version | 0.7.0 (interface, models M1 and M2, campaign rules of the frozen analysis plan and of its update 1, analytic predictor; simulator of M1 and M2 for a web browser in `web/`) |
| Specification | interface specification of the SIM-ACI project, version 0.2, approved by the author on 2026-11-05 (`wp1_replication/interface_spec.md` of the project repository); it is not distributed with the package or with the reproduction archive of the first article |
| Status | public repository <https://github.com/Ph-Terrier/walkbench>; each release is archived on Zenodo through the link between GitHub and Zenodo; version 0.6.2 (doi:10.5281/zenodo.24354489) is the version of the preprint of the first article of SIM-ACI, and the article prepared for a journal uses version 0.7.0 |

## Purpose

walkbench exposes walking models through one interface, so that an experiment written for one model runs on the others without change. The interface has six parts:

- a perturbation port that applies any input series, one value per cycle, at a named injection point;
- a closed-loop port, called at each contact, for corrective controllers and a metronome;
- a linearization at the fixed point, by central finite differences;
- standardized outputs of a run;
- common estimators: short-term and long-term divergence exponents with the Rosenstein engine of RACING v1.0.2, Floquet multipliers, kinematic variability, detrended fluctuation analysis (DFA);
- a run driver with a seed rule, redraws after falls and a run log.

Version 0.7.0 contains two models. M1 is the simplest walker of Garcia et al. (1998) with the foot-strike perturbations of Su and Dingwell (2007), as implemented in the feasibility study of SIM-ACI. M2 is the walker with arced feet and hip spring of Hobbelen and Wisse (2007), used by Bruijn et al. (2012), perturbed by the height of the floor at each step. Its equations were derived by the SP1.1 session and checked against the printed ones (`wp1_replication/04_m2_equations_check.py` of the SIM-ACI project). The option `impact = "printed"` of `model_m2()` applies the impact map as printed by Hobbelen and Wisse (2007, p. 1223). It differs from the derived map only after a step of the floor and when the foot radius is not zero.

Each model searches its fixed point at the first call of `fixed_point()`. The search of M1 is the frozen function `find_fixed_point()` of the pinned walker file. It runs Newton iterations from a fixed initial guess and returns the last iterate, without a test of convergence or stability; it stops only when the walker falls during an iteration. The search of M2 stops with an error when it does not converge, and it does not test stability. The field `eig` of `fixed_point()` holds the eigenvalues of the linearized step map, and the cycle is stable when all their moduli are below 1. On 2026-11-13, `model_m1(gamma = 0.02)` and `model_m2(gamma = 0.02)` returned unstable fixed points without an error (largest moduli 5.4 and 1.9). The tests of the package check these moduli for M1 at a slope of 0.009 rad and for M2 at slopes of 0.004 and 0.009 rad. The configurations of M2 in the tests include that of the first article (slope 0.004 rad, r_f = 0.45, k = 5). The campaigns of the first article used M1 at a slope of 0.009 rad and M2 in this configuration.

The other parts of the package apply the analysis plan of SIM-ACI, frozen at commit ba72afe and registered on OSF Registries (<https://doi.org/10.17605/OSF.IO/M74TK>), with its updates 1 and 2:

- Estimation settings. `settings_m2()` gives the primary settings of M2, those of table 5.1 of the plan. The state variables of the model are referred to the stance leg, and each step is interpolated separately. The search for nearest neighbors excludes the points within half a stride, as in the public code of S. M. Bruijn. `settings_m2("continuous")` gives the settings of the replication runs of M2.
- Transient. `transient_cycles()` applies the rule of section 4.3 of the plan: 20 cycles in M1, and 34 in M2 in the configuration of section 4.10 (slope 0.004 rad, r_f = 0.45, k = 5). The section "Example" shows how to reach the fixed point of M2 in this configuration. `transient_cycles_loop()` gives the transient under a controller.
- Campaign rules. The package gives the master seeds of the seven campaigns and rescales the inputs after the transient. It starts the real stride series and their surrogates in a circular way, and it applies the DFA settings of Terrier (2026). `summarize_cells()` gives the fall rate and the inclusion of each cell.
- Numerical failures. `run_trial()` counts a failure of the integrator apart from a fall (update 1, section 4). `summarize_cells()` flags a cell when its fall rate, or its combined rate of falls and failures, exceeds 20%.
- Analytic predictor. `predictor_levels()` gives the expected divergence curve from the linearization of a model and the autocovariance of its input. `predictor_settings_m2()` gives the settings of updates 1 and 2 for M2. Since version 0.7.0, the functions of the predictor have names with the prefix `predictor_`; their former names, such as `predict_spinal_levels()`, stay valid.

`NEWS.md` describes the changes of each version.

## Installation

Version 0.7.0 is installed from GitHub at its tag with the package remotes:

```r
# install.packages("remotes")
remotes::install_github("Ph-Terrier/walkbench@v0.7.0")
```

Version 0.6.2, the version of the preprint of the first article of SIM-ACI, is installed in the same way with the tag `v0.6.2`.

The following call installs a local copy of the source, for example the copy in the reproduction archive of the first article:

```r
install.packages("path/to/walkbench", repos = NULL, type = "source")
```

The package needs R 4.5.0 or later. `load_racing()` and the pinned walker file check the RACING file against its MD5 sum, computed with the argument `bytes` of `tools::md5sum()`, which R 4.5.0 introduced. The package contains R code only, so its installation needs no compiler. `library(walkbench)` then loads it.

Under Windows, the path of the R library should have at most about 150 characters. During the installation, the path of one pinned file adds about 107 characters to the path of the library, and the default limit of a path under Windows is 260 characters. With a longer library path, R gives only a warning, and the installed package lacks the pinned file `07b_walker_prediction.R`. `check_pinned_files()` then stops with an error.

## Loading

The package imports deSolve, digest, jsonlite, MASS and RANN. To load it without installing it, run from the folder that contains its source:

```r
pkgload::load_all()
```

`devtools::load_all()` calls the same function. The campaign scripts of the first article of SIM-ACI load the package in this way, and their run logs record its version.

## Example

```r
m1 <- model_m1()
lin <- linearize(m1)                     # x*, T*, J, b, a, e, static gain, sensitivities
inp <- input_arfima(620, d = 0.3, sd = 0.003, seed = 1)
run <- trim_run(simulate_run(m1, inp, n_cycles = 620), n_transient = 20)
row <- estimate_all(run, m1, input = inp)
row[, c("lambda_s", "lambda_l", "alpha_duration")]

# closed loop of variant H3b: correction of the asynchrony with a metronome
cs <- input_arfima(620, d = 0.45, sd = 0.003, seed = 2)
run_b <- simulate_run(m1, n_cycles = 620, metronome = make_metronome(lin$period),
                      controller = make_controller_h3b(cs, kappa = 0.2, lin))
```

The first article used M2 with a slope of 0.004 rad, r_f = 0.45 and k = 5. In this configuration, the default initial guess of `model_m2()` does not reach the fixed point, and `model_m2(gamma = 0.004, r_f = 0.45, k = 5)$fixed_point()` stops with "fixed point of M2 not found". The campaign scripts pass the fixed point to `model_m2()` as `x_init`, read from an output file of the project. Continuation in r_f and k reaches the same fixed point from the starting configuration (r_f = 0, k = 0). The foot radius rises by steps of 0.05, then the hip stiffness by steps of 0.5, and each fixed point serves as the initial guess of the next step:

```r
x <- model_m2(gamma = 0.004)$fixed_point()$x_star
for (r_f in seq(0.05, 0.45, by = 0.05)) {
  x <- model_m2(gamma = 0.004, r_f = r_f, x_init = x)$fixed_point()$x_star
}
for (k in seq(0.5, 5, by = 0.5)) {
  x <- model_m2(gamma = 0.004, r_f = 0.45, k = k, x_init = x)$fixed_point()$x_star
}
m2 <- model_m2(gamma = 0.004, r_f = 0.45, k = 5, x_init = x)
transient_cycles(m2)                     # 34
```

On 2026-11-12, these calls took 8 s on the development laptop. They gave the fixed point of the campaigns of the first article to 1e-13. The script `wp2_injection/01_m2_configuration_search.R` of the SIM-ACI project follows the same path. It also follows a second path, with the hip stiffness first, which reaches the same cycle.

## Main functions

| Part | Functions |
|---|---|
| models | `model_m1()`, `model_m2()`, `list_models()` |
| perturbation port | `simaci_input()`, `input_iid_uniform()`, `input_arfima()`, `input_fixed_series()`, `input_surrogate()`, `seed_surrogate()`, `check_input()` |
| closed-loop port | `make_metronome()`, `make_controller_h3a()`, `make_controller_h3b()`, `gain_scale()`, `closed_loop_matrix()`, `closed_loop_radius()`, `stable_gain_limit()` |
| linearization | `linearize()` |
| runs | `simulate_run()`, `trim_run()`, `transient_cycles()`, `transient_cycles_loop()`, `transient_from_radius()`, `run_trial()`, `draw_single_seed()`, `draw_fixed()`, `seed_campaign()`, `check_master_seeds()`, `write_campaign_log()`, `check_output_folder()` |
| campaign rules | `campaign_table()`, `campaign_master()`, `number_cells()`, `bootstrap_seed()`, `check_seed_plan()`, `bind_trials()`, `summarize_cells()` |
| estimators | `settings_m2()`, `estimate_divergence()`, `estimate_floquet()`, `reference_xstar()`, `estimate_variability()`, `dfa_settings()`, `estimate_dfa()`, `estimate_all()` |
| analytic predictor | `fi_autocorrelation()`, `ar1_autocorrelation()`, `centered_autocovariance()`, `sample_autocovariance()`, `predictor_d_grid()`, `predictor_levels()`, `predictor_lambda_l_m1()`, `predictor_lambda_l_lsq()`, `predictor_n_past()`, `predictor_settings_m2()` (former names `spinal_d_grid()`, `predict_spinal_levels()`, `spinal_lambda_l_m1()`, `spinal_lambda_l_lsq()`, `spinal_n_past()`, `spinal_settings_m2()`), `window_slope_half_log_v()`, `match_fi_to_ar1()` |
| pinned code | `pinned_file()`, `check_pinned_files()`, `load_racing()` |

## User guide

The folder `docs/` holds the user guide of version 0.7.0, `docs/walkbench_user_guide.pdf`, dated 21 November 2026, and the example scripts that produced it, in `docs/scripts/`. The first chapters of the guide cover the installation and a first session, the architecture and conventions, and the models M1 and M2. The next chapters present the perturbation port, runs and trials, the linearization at the fixed point, the closed-loop port and the estimators. Chapters 11 to 13 cover the campaign rules and the run log, the analytic predictor, and the pinned code and tests. Chapter 14 presents the web simulator of the folder `web/`. The appendices give an index of the functions, the fields of the objects and a glossary, and they describe the example scripts. From the folder `docs/`, the call `Rscript scripts/00_run_example.R scripts/06_perturbation_port.R` runs the script of chapter 6 and writes its outputs to `docs/generated/`. The driver loads the package from the source of the repository with pkgload. The guide and its scripts describe version 0.7.0, and the driver stops with another version of the package. The guide of version 0.6.2 and its scripts stay in the repository at the tag `v0.6.2`. Chapters 1 to 13, the appendices and their scripts were written by the guide session of SIM-ACI (Claude Fable 5.2). A second model, Claude Opus 5.5, checked them against the code of the package and wrote part of the revisions. Chapter 14 and its script were written by a writer agent (Claude Opus 5.5) and revised by the guide session. The SP1.1 session adapted the paths of the scripts to this repository. The file `.Rbuildignore` excludes the folder `docs/`, so the source tarball of the package and the installed package do not contain the guide.

## Web simulator

The folder `web/` holds a simulator of the models M1 and M2 that runs in a web browser, without installation (`web/README.md`). The visitor sets the amplitude and the memory parameter of an ARFIMA perturbation, or loads a series, and watches the walker. A reference run of 600 steps gives the outcomes of `estimate_all()` and writes the R code that reruns the same run in walkbench. The models and estimators of the page were checked against walkbench 0.7.0 with test vectors (`web/validation/validation_m1.md`, `web/validation/validation_m2.md`). The page is a teaching demonstration; the results of the SIM-ACI project come from the R package. The workflow `.github/workflows/pages.yml` publishes the folder `web/` on GitHub Pages. The page has been online at <https://ph-terrier.github.io/walkbench/> since 2026-11-21. The file `.Rbuildignore` excludes the folders `web/` and `.github/`, so the source tarball of the package and the installed package do not contain the simulator.

## Pinned code

Code taken from other repositories is copied unchanged into `inst/pinned/`, under its path in the repository of origin. `inst/pinned/MANIFEST.csv` gives for each file the repository, commit and path of origin, the git blob, the SHA-256 sum with LF line endings, the license, the date and the producer of the copy. The package recomputes the sum before it uses a file and stops when the sum differs. `.gitattributes` exempts `inst/pinned/` from any conversion of line endings.

| Pinned file | Origin | Use |
|---|---|---|
| `simaci/feasibility/scripts/00_functions_walker.R` | repository of the SIM-ACI project, git blob `7c20ffd1` | functions of M1 |
| `simaci/feasibility/scripts/racing/rosenstein_divergence.R` | RACING v1.0.2, git blob `47aabf92`, MIT license | Rosenstein engine |
| `simaci/feasibility/scripts/racing/LICENSE`, `README.md` | RACING v1.0.2 and the repository of the SIM-ACI project | license and provenance of the engine |
| `time_series_R/SPINAL/validation-aci/checks/07b_walker_prediction.R` | repository "time series R" of the author, commit ced76be, git blob `12fe2148` | reference of the analytic predictor (candidate form) |

The citation of RACING is in `inst/pinned/simaci/feasibility/scripts/racing/README.md`. The functions of RACING are loaded into a private environment and are not exported.

## Tests

From the folder that contains the source of the package, `testthat::test_local()` runs the 82 test blocks of the package. On 2026-11-20, a run took 105 s on the development laptop (Windows 11, R 4.5.1). They compare the cycle loop and the estimators with the frozen functions of the pinned walker file. They check the linearization of M1 against reference values from another project of the author, which the tests contain. They also check the pinned files, the closed-loop port, the seed rule, the conformance of every model adapter, the autocorrelation of the ARFIMA series and the file `runs.csv` of the run log. One comparison reads `compute_dfa_alpha()` of the Zenodo archive of Terrier (2026) from a local copy in the repository "time series R" of the author. Without that copy, this comparison is skipped, and the same test still checks the stored reference values. A reader of the package can thus run every test block.

Two scripts of the SIM-ACI project check the package against earlier results and are not part of the package. `wp1_replication/01_replay_feasibility.R` replays the 150 trials of the feasibility study; the reproduction archive of the first article contains it with its inputs. `wp1_replication/02_compare_spinal_linearization.R` compares the full linearization of M1 with a file of another project of the author, which is not public. The archive contains its two output files.

## Files of the SIM-ACI project named in the code

Paths that start with `SIMACI/` appear in some headers and comments of `R/` and `tests/`, in the help page of `spinal_settings_m2()` and in `NEWS.md`. These paths name files of the repository of the SIM-ACI project, which is not public. The reproduction archive of the first article contains the campaign scripts of that article, whose run logs record the version of walkbench and the commit of the project repository from which it was loaded.

## Producers of the code

The responsible author is Philippe Terrier. The R code of `R/`, the tests and this README were written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a code-generation model. They follow a specification drafted by the same session and approved by the author. The pinned walker file comes from the feasibility study of SIM-ACI; RACING v1.0.2 is the author's published package. The simulator of `web/` was written by the web-simulator session of SIM-ACI (Claude Opus 5.5), without a code-generation model. The user guide of `docs/` and its example scripts were written by the guide session of SIM-ACI (Claude Fable 5.2). A writer agent (Claude Opus 5.5) wrote chapter 14 of the guide, on the simulator, and its script; the guide session revised them.

## Citation

A citation of walkbench gives the author, the title, the version and the year, for example: Terrier P (2026). walkbench: Testbed of Walking Models for the Divergence Exponents of Gait, version 0.7.0. Each release on GitHub is archived on Zenodo, and the Zenodo record of a release gives the DOI of that version. The concept DOI doi:10.5281/zenodo.24354488 names all the versions and resolves to the latest one. `CITATION.cff` gives the same information in a machine-readable form.

The preprint of the first article of SIM-ACI used version 0.6.2 (doi:10.5281/zenodo.24354489), and the article prepared for a journal uses version 0.7.0. The preprint is: Terrier P (2026). The long-term divergence exponent and the temporal structure of step-to-step perturbations: a simulation study in two passive walking models. arXiv:2611.26182, doi:10.48550/arXiv.2611.26182.

## License

MIT, Copyright (c) 2026 Philippe Terrier (`LICENSE`, `LICENSE.md`). The pinned RACING file keeps its own MIT license (`inst/pinned/simaci/feasibility/scripts/racing/LICENSE`).

The text and the figures of the user guide `docs/walkbench_user_guide.pdf` are distributed under the Creative Commons Attribution 4.0 International license (CC BY 4.0, <https://creativecommons.org/licenses/by/4.0/>). The code of its examples is distributed under the MIT license of the package.

## References

- Garcia M, Chatterjee A, Ruina A, Coleman M (1998). The simplest walking model: stability, complexity, and scaling. *Journal of Biomechanical Engineering*, 120, 281-288. doi:10.1115/1.2798313
- Su JL-S, Dingwell JB (2007). Dynamic stability of passive dynamic walking on an irregular surface. *Journal of Biomechanical Engineering*, 129, 802-810. doi:10.1115/1.2800760
- Hobbelen DGE, Wisse M (2007). A disturbance rejection measure for limit cycle walkers: the gait sensitivity norm. *IEEE Transactions on Robotics*, 23, 1213-1224. doi:10.1109/TRO.2007.904908
- Bruijn SM, Bregman DJJ, Meijer OG, Beek PJ, van Dieën JH (2012). Maximum Lyapunov exponents as predictors of global gait stability: a modelling approach. *Medical Engineering & Physics*, 34, 428-436. doi:10.1016/j.medengphy.2011.07.024
- Terrier P (2026). Persistent and anti-persistent stride-to-stride fluctuations: an ARFIMA decomposition consistent with closed-loop sensorimotor control. *Physica A: Statistical Mechanics and its Applications*, 699, 131842. doi:10.1016/j.physa.2026.131842
