# Title: Test vectors of model M1 for the JavaScript port of the web simulator
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
#   (Claude Opus 5.5), without a code-generation model
# Date: 2026-11-19
# Description: loads walkbench from the source tree of the package (the folder two levels above
#   this script) and writes the reference values that the JavaScript port of M1 must
#   reproduce: the fixed point and the linearization; ARFIMA inputs with the normal draws that
#   produced them; a fixed series with its circular start; a run of 620 steps (20 transient
#   steps and 600 kept steps) with its outcomes; a reference computed by the frozen functions
#   of M1 at tight lsodar tolerances (fixed point and the whole run of 620 steps); uniform and
#   normal draws of R for five seeds; a short run with its whole trajectory, for the test of
#   the estimators on the same samples; a run that falls; DFA cases. The tolerances are set in
#   tests/run_tests_m1.js. The script reads the package and writes no file inside it.
#   Changes of 2026-11-19 (same session): reference at tight tolerances (fixed point and
#   whole main run) and draws of R, after the decisions of the author on the tolerances and
#   on the export of the page.
# Dependencies: pkgload, jsonlite, digest; walkbench and its imports (deSolve, RANN, MASS)
# Input: the source of walkbench (folder testbed/ of the SIM-ACI repository); the optional
#   environment variable WALKBENCH_GIT gives the git executable for the record of the commit
# Output: web/tests/vectors/m1_vectors.json
# Usage: Rscript tools/01_make_vectors_m1.R (the paths are resolved from the location of the
#   script, so the working folder does not matter)

library(pkgload)
library(jsonlite)
library(digest)

# --- Paths, from the location of this script ---------------------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args_all, value = TRUE))
if (length(file_arg) != 1L) stop("run this script with Rscript")
script_dir <- dirname(normalizePath(file_arg, winslash = "/"))
web_dir <- dirname(script_dir)
pkg_dir <- dirname(web_dir)
out_dir <- file.path(web_dir, "tests", "vectors")
out_file <- file.path(out_dir, "m1_vectors.json")

pkgload::load_all(pkg_dir, quiet = TRUE)
t_start <- Sys.time()

# --- Record of the code that produced the vectors -------------------------------------------
hashed <- c("DESCRIPTION", "R/model_m1.R", "R/perturbation_port.R", "R/estimators.R",
            "R/racing.R", "R/run_driver.R", "R/linearization.R",
            "inst/pinned/simaci/feasibility/scripts/00_functions_walker.R",
            "inst/pinned/simaci/feasibility/scripts/racing/rosenstein_divergence.R")
sha256 <- vapply(hashed, function(f) digest::digest(file = file.path(pkg_dir, f), algo = "sha256"),
                 character(1))
git <- git_state(pkg_dir)
vers <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)

meta <- list(
  produced_by = paste("tools/01_make_vectors_m1.R, written by the orchestrating model of the",
                      "web-simulator session of SIM-ACI (Claude Opus 5.5)"),
  walkbench_version = vers("walkbench"),
  loaded_from = "source tree, pkgload::load_all()",
  git_commit = git$commit, git_modified_files_in_package = git$modified,
  sha256_of_working_tree_files = as.list(sha256),
  r_version = R.version.string, platform = R.version$platform, rng_kind = RNGkind(),
  package_versions = list(deSolve = vers("deSolve"), RANN = vers("RANN"), MASS = vers("MASS"),
                          digest = vers("digest"), jsonlite = vers("jsonlite"),
                          pkgload = vers("pkgload")),
  date = format(t_start, "%Y-%m-%dT%H:%M:%S%z"))

# --- Model, fixed point, linearization ------------------------------------------------------
m1 <- model_m1()
fp <- m1$fixed_point()
lin <- linearize(m1)
eig_list <- function(e) list(re = Re(e), im = Im(e), mod = Mod(e))

fixed_point <- c(list(params = m1$params, x_star = fp$x_star, period = fp$period, y0 = fp$y0),
                 list(eig = eig_list(fp$eig)))
linearization <- list(h = lin$h, x_star = lin$x_star, period = lin$period,
                      j_map_by_row = as.vector(t(lin$j_map)), b_vec = lin$b_vec,
                      a_vec = lin$a_vec, e_sc = lin$e_sc, static_gain = lin$static_gain,
                      eig = eig_list(lin$eig), impulse_T = lin$impulse_T)

# --- ARFIMA inputs: normal draws z of set.seed(seed), and the values of input_arfima() ------
arfima_case <- function(n, d, sd, seed, n_transient) {
  set.seed(seed)
  z <- stats::rnorm(n)  # the draws of the frozen arfima_series(), which calls set.seed(seed)
  inp <- input_arfima(n, d, sd, seed = seed, n_transient = n_transient)
  list(n = n, d = d, sd = sd, seed = seed, n_transient = n_transient, z = z,
       values = inp$values)
}
arfima <- list(arfima_case(620, 0.30, 0.00234, 1, 20), arfima_case(620, -0.30, 0.00234, 3, 20),
               arfima_case(620, 0.00, 0.00234, 4, 20), arfima_case(620, 0.45, 0.00234, 5, 20),
               arfima_case(300, 0.20, 0.00100, 6, 0))

# --- Fixed series with a circular start (synthetic AR(1) series, no third-party data) -------
set.seed(7)
w <- as.numeric(stats::filter(stats::rnorm(300), 0.6, method = "recursive"))
fs <- input_fixed_series(w, n_transient = 20, sd = 0.002)
fixed_series <- list(window = w, n_transient = 20, sd = 0.002, values = fs$values)

# --- Main run: 620 steps, d = 0.3, sigma_ref of M1 (2.34e-3 rad), seed 1 --------------------
outcome_names <- c("mean_duration", "sd_duration", "cv_duration", "lambda_s", "lambda_l",
                   "lambda_s_per_step", "lambda_l_per_step", "n_pairs", "alpha_input",
                   "alpha_command", "alpha_duration", "alpha_stride_duration")
inp <- input_arfima(620, d = 0.30, sd = 0.00234, seed = 1, n_transient = 20)
t_run <- Sys.time()
run <- simulate_run(m1, inp, n_cycles = 620)
wall_sim <- as.numeric(difftime(Sys.time(), t_run, units = "secs"))
if (run$fell) stop("the main run fell; choose another seed")
tr <- trim_run(run, 20)
t_est <- Sys.time()
row <- estimate_all(tr, m1, input = inp)
wall_est <- as.numeric(difftime(Sys.time(), t_est, units = "secs"))
dv <- attr(row, "divergence")
smp <- run$samples
subset_cycles <- c(1, 2, 310, 311, 619, 620)
sub <- smp[smp[, "cycle"] %in% subset_cycles, , drop = FALSE]
ev <- run$events
main_run <- list(
  n_cycles = 620, n_transient = 20, d = 0.30, sd = 0.00234, seed = 1, input = inp$values,
  y0 = fp$y0, fell = run$fell, n_cycles_done = run$n_cycles_done,
  events = list(t_start = ev$t_start, t_end = ev$t_end, duration = ev$duration, u = ev$u,
                leg = ev$leg, theta_post = ev$theta_post, theta_dot_post = ev$theta_dot_post),
  samples_n = nrow(smp),
  samples_colsum = as.list(colSums(smp[, c("thG", "thGd", "phG", "phGd")])),
  samples_subset = list(cycles = subset_cycles, t = sub[, "t"], thG = sub[, "thG"],
                        thGd = sub[, "thGd"], phG = sub[, "phG"], phGd = sub[, "phGd"],
                        cycle = sub[, "cycle"]),
  outcomes = as.list(row[1, outcome_names]),
  divergence = list(mld = dv$mld, mld_smooth = dv$mld_smooth, slopes = dv$slopes,
                    time = dv$time, n_horizon = dv$n_horizon, theiler = dv$theiler,
                    n_pairs = dv$n_pairs),
  wall_time_s = list(simulate_run = wall_sim, estimate_all = wall_est))

# --- Reference at tight tolerances (diagnostic of the integration error of walkbench) ---------
# The frozen functions of M1 (pinned walker file) called from this script with lsodar
# tolerances rtol 1e-13 and atol 1e-15, in place of the defaults 1e-10 and 1e-12 of
# integrate_step(); the package is not changed. Added on 2026-11-19 after the first run of
# tests/run_tests_m1.js, whose fixed point differed from that of walkbench by 1.9e-9.
w <- m1_walker_env()
tight_rtol <- 1e-13
tight_atol <- 1e-15
step_map_tight <- function(x) {
  y0 <- c(x[1], x[2], 2 * x[1], x[2] * (1 - cos(2 * x[1])))
  st <- w$integrate_step(y0, m1$params$gamma, eps = 0, dt = 1, rtol = tight_rtol, atol = tight_atol)
  y_new <- w$heelstrike_map(st$y_end)
  list(x_next = y_new[1:2], period = st$t_end)
}
x <- c(0.2, -0.2)
for (i in seq_len(50)) {
  f0 <- step_map_tight(x)$x_next - x
  if (sqrt(sum(f0^2)) < 1e-12) break
  jac <- matrix(0, 2, 2)
  for (j in 1:2) {
    xp <- x
    xp[j] <- xp[j] + 1e-7
    jac[, j] <- (step_map_tight(xp)$x_next - xp - f0) / 1e-7
  }
  x <- x - solve(jac, f0)
}
# the whole main run (620 steps, same input and same initial state) at the tight tolerances;
# extended to the whole run on the author's decision of 2026-11-19 (cumulative times tested
# against this reference at 1e-7)
# (sampled on the global grid, as m1_simulate_loop(); samples kept for the steps of the subset,
# on the author's decision of 2026-11-19 to test the sampled states against this reference)
y <- fp$y0
n_main <- length(inp$values)
d_tight <- t_start_tight <- t_end_tight <- th_post_tight <- thd_post_tight <- numeric(n_main)
sub_tight <- list()
t_glob <- 0
stance <- 1L
for (k in seq_len(n_main)) {
  th_post_tight[k] <- y[1]
  thd_post_tight[k] <- y[2]
  st <- w$integrate_step(y, m1$params$gamma, inp$values[k], dt = m1$params$dt,
                         t_offset = t_glob, t_min = m1$params$t_min, rtol = tight_rtol,
                         atol = tight_atol)
  if (st$fell) stop("the main run fell at the tight tolerances")
  if (k %in% subset_cycles) {
    sm <- st$samples
    sub_tight[[length(sub_tight) + 1L]] <- cbind(t = sm[, 1] + t_glob,
                                                 w$to_continuous(sm[, 2:5, drop = FALSE], stance))
  }
  d_tight[k] <- st$t_end
  t_start_tight[k] <- t_glob
  t_end_tight[k] <- t_glob + st$t_end
  t_glob <- t_glob + st$t_end
  y <- w$heelstrike_map(st$y_end, variant = "timing", eps = inp$values[k])
  stance <- 3L - stance
}
sub_tight <- do.call(rbind, sub_tight)
tight_reference <- list(rtol = tight_rtol, atol = tight_atol, x_star = x,
                        period = step_map_tight(x)$period,
                        main_run = list(duration = d_tight, t_start = t_start_tight,
                                        t_end = t_end_tight, theta_post = th_post_tight,
                                        theta_dot_post = thd_post_tight,
                                        samples_subset = list(t = sub_tight[, "t"],
                                                              thG = sub_tight[, "thG"],
                                                              thGd = sub_tight[, "thGd"],
                                                              phG = sub_tight[, "phG"],
                                                              phGd = sub_tight[, "phGd"])))

# --- Random numbers of R (Mersenne-Twister, Inversion), for the generator of the page ---------
rng <- lapply(c(1, 2, 42, 123456, 999999), function(s) {
  set.seed(s)
  u <- stats::runif(10)
  set.seed(s)
  z <- stats::rnorm(1000)
  list(seed = s, runif = u, rnorm = z)
})

# --- Short run: whole trajectory, for the estimators on the same samples --------------------
inp_s <- input_arfima(130, d = 0.30, sd = 0.00234, seed = 2, n_transient = 20)
run_s <- simulate_run(m1, inp_s, n_cycles = 130)
if (run_s$fell) stop("the short run fell; choose another seed")
tr_s <- trim_run(run_s, 20)
X <- unname(as.matrix(tr_s$samples[, c("thG", "thGd", "phG", "phGd")]))
dv_s <- estimate_divergence(tr_s, settings_m1(), 2L)
alt <- settings_m1()
alt$window_s <- c(0, 0.2)
alt$window_l <- c(4, 6)
alt$horizon <- 6
dv_alt <- estimate_divergence(tr_s, alt, 2L)
div_out <- function(d) list(mld = d$mld, mld_smooth = d$mld_smooth, slopes = d$slopes,
                            time = d$time, lambda_s = d$lambda_s, lambda_l = d$lambda_l,
                            lambda_s_per_step = d$lambda_s_per_step,
                            lambda_l_per_step = d$lambda_l_per_step, n_pairs = d$n_pairs,
                            n_horizon = d$n_horizon, theiler = d$theiler)
short_run <- list(
  n_cycles = 130, n_transient = 20, d = 0.30, sd = 0.00234, seed = 2, input = inp_s$values,
  dt = tr_s$dt, durations = tr_s$events$duration, n_samples = nrow(X),
  samples_by_row = as.vector(t(X)),
  settings_primary = list(window_s = c(0, 0.1), window_l = c(3, 4), horizon = 4, theiler = 2),
  divergence_primary = c(div_out(dv_s), list(neighbors = dv_s$neighbors)),
  settings_alternative = list(window_s = alt$window_s, window_l = alt$window_l,
                              horizon = alt$horizon, theiler = alt$theiler),
  divergence_alternative = div_out(dv_alt))

# --- A run that falls: first seed from 1001 whose run falls after step 25 -------------------
fall_run <- NULL
for (s in 1001:1200) {
  inp_f <- input_arfima(620, d = 0, sd = 0.006, seed = s, n_transient = 20)
  r <- simulate_run(m1, inp_f, n_cycles = 620)
  if (r$fell && r$fall_cycle > 25) {
    fall_run <- list(n_cycles = 620, d = 0, sd = 0.006, seed = s, input = inp_f$values,
                     fell = r$fell, fall_cycle = r$fall_cycle, n_cycles_done = r$n_cycles_done,
                     durations = r$events$duration)
    break
  }
}
if (is.null(fall_run)) stop("no run fell between seeds 1001 and 1200")

# --- DFA cases (configuration of the analysis plan, dfa_settings()) -------------------------
set.seed(11)
x_white <- stats::rnorm(600)
set.seed(12)
x_walk <- cumsum(stats::rnorm(400))
set.seed(13)
x_short <- stats::rnorm(95)
d_kept <- tr$events$duration
strides <- colSums(matrix(d_kept[seq_len(2 * (length(d_kept) %/% 2))], nrow = 2))
dfa_case <- function(label, x) list(label = label, x = x, alpha = estimate_dfa(x))
dfa <- list(dfa_case("white noise, 600 values", x_white),
            dfa_case("random walk, 400 values", x_walk),
            dfa_case("white noise, 95 values (too short)", x_short),
            dfa_case("step durations of the main run", d_kept),
            dfa_case("stride durations of the main run", strides))

# --- Write ------------------------------------------------------------------------------------
meta$wall_time_s <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
vectors <- list(meta = meta, fixed_point = fixed_point, linearization = linearization,
                arfima = arfima, fixed_series = fixed_series, main_run = main_run,
                tight_reference = tight_reference, rng = rng, short_run = short_run,
                fall_run = fall_run, dfa = dfa)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(vectors, out_file, auto_unbox = TRUE, digits = I(17), na = "null",
                     null = "null", pretty = FALSE)
cat(sprintf("written: %s (%.1f MB) in %.0f s\n", out_file, file.size(out_file) / 1e6,
            meta$wall_time_s))
cat(sprintf("main run: lambda_s %.6f, lambda_l %.6f, alpha_duration %.4f; fall run: seed %d, fall at step %d\n",
            row$lambda_s, row$lambda_l, row$alpha_duration, fall_run$seed, fall_run$fall_cycle))
