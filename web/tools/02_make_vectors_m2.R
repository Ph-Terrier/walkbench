# Title: Test vectors of model M2 for the JavaScript port of the web simulator
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
#   (Claude Opus 5.5), without a code-generation model
# Date: 2026-11-19
# Description: loads walkbench from the source tree of the package (the folder two levels above
#   this script) and writes the reference values that the JavaScript port of M2 must reproduce,
#   in the configuration of the injection experiments of the analysis plan (section 4.10:
#   slope 0.004 rad, r_f = 0.45, k = 5): the fixed point, reached by the continuation of the
#   README of walkbench, and the linearization; the fixed point of the starting configuration
#   (r_f = 0, k = 0); a reference computed by the functions of M2 at tight lsodar tolerances
#   (fixed point and a whole run of 634 steps); a run of 634 steps (34 transient steps and 600
#   kept steps) with its outcomes; a short run with its trajectory, the bounds of its steps and
#   its resampled state, for the test of the estimators on the same samples; a run that falls.
#   The tolerances are set in tests/run_tests_m2.js, with the two levels approved by the author
#   for M1 on 2026-11-19. The script reads the package and writes no file inside it.
#   Change of 2026-11-20 by the same model: the main run also records n_grid, the number of
#   points of the resampling grid given by grid_count() of walkbench 0.7.0; the script then
#   needs walkbench 0.7.0 or later.
# Dependencies: pkgload, jsonlite, digest; walkbench and its imports (deSolve, RANN, MASS)
# Input: the source of walkbench (folder testbed/ of the SIM-ACI repository); the optional
#   environment variable WALKBENCH_GIT gives the git executable for the record of the commit
# Output: web/tests/vectors/m2_vectors.json
# Usage: Rscript tools/02_make_vectors_m2.R (paths resolved from the location of the script)

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
out_file <- file.path(out_dir, "m2_vectors.json")

pkgload::load_all(pkg_dir, quiet = TRUE)
t_start <- Sys.time()

# --- Record of the code that produced the vectors -------------------------------------------
hashed <- c("DESCRIPTION", "R/model_m2.R", "R/model_m1.R", "R/perturbation_port.R",
            "R/estimators.R", "R/racing.R", "R/run_driver.R", "R/linearization.R",
            "inst/pinned/simaci/feasibility/scripts/00_functions_walker.R",
            "inst/pinned/simaci/feasibility/scripts/racing/rosenstein_divergence.R")
sha256 <- vapply(hashed, function(f) digest::digest(file = file.path(pkg_dir, f), algo = "sha256"),
                 character(1))
git <- git_state(pkg_dir)
vers <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)
meta <- list(
  produced_by = paste("tools/02_make_vectors_m2.R, written by the orchestrating model of the",
                      "web-simulator session of SIM-ACI (Claude Opus 5.5)"),
  walkbench_version = vers("walkbench"), loaded_from = "source tree, pkgload::load_all()",
  git_commit = git$commit, git_modified_files_in_package = git$modified,
  sha256_of_working_tree_files = as.list(sha256), r_version = R.version.string,
  platform = R.version$platform, rng_kind = RNGkind(),
  package_versions = list(deSolve = vers("deSolve"), RANN = vers("RANN"), MASS = vers("MASS"),
                          digest = vers("digest"), jsonlite = vers("jsonlite"),
                          pkgload = vers("pkgload")),
  date = format(t_start, "%Y-%m-%dT%H:%M:%S%z"))

eig_list <- function(e) list(re = Re(e), im = Im(e), mod = Mod(e))
gamma <- 0.004
r_f <- 0.45
k_hip <- 5

# --- Starting configuration (r_f = 0, k = 0) and continuation (README of walkbench) ----------
m0 <- model_m2(gamma = gamma)
fp0 <- m0$fixed_point()
x <- fp0$x_star
for (rr in seq(0.05, 0.45, by = 0.05)) {
  x <- model_m2(gamma = gamma, r_f = rr, x_init = x)$fixed_point()$x_star
}
for (kk in seq(0.5, 5, by = 0.5)) {
  x <- model_m2(gamma = gamma, r_f = r_f, k = kk, x_init = x)$fixed_point()$x_star
}
m2 <- model_m2(gamma = gamma, r_f = r_f, k = k_hip, x_init = x)
fp <- m2$fixed_point()
lin <- linearize(m2)
n_tr <- transient_cycles(m2)
start_config <- list(params = m0$params, x_star = fp0$x_star, period = fp0$period,
                     eig = eig_list(fp0$eig))
fixed_point <- list(params = m2$params, x_init_used = x, x_star = fp$x_star,
                    period = fp$period, step_length = fp$step_length, eig = eig_list(fp$eig),
                    transient_cycles = n_tr)
linearization <- list(h = lin$h, j_map_by_row = as.vector(t(lin$j_map)), b_vec = lin$b_vec,
                      a_vec = lin$a_vec, e_sc = lin$e_sc, static_gain = lin$static_gain,
                      eig = eig_list(lin$eig))

# --- Main run: 634 steps, d = 0.3, sigma_ref of M2 (1.8e-4 leg lengths), seed 1 -------------
outcome_names <- c("mean_duration", "sd_duration", "cv_duration", "lambda_s", "lambda_l",
                   "lambda_s_per_step", "lambda_l_per_step", "n_pairs", "alpha_input",
                   "alpha_command", "alpha_duration", "alpha_stride_duration")
n_main <- n_tr + 600L
inp <- input_arfima(n_main, d = 0.30, sd = 1.8e-4, seed = 1, n_transient = n_tr)
t_run <- Sys.time()
run <- simulate_run(m2, inp, n_cycles = n_main)
wall_sim <- as.numeric(difftime(Sys.time(), t_run, units = "secs"))
if (run$fell) stop("the main run fell; choose another seed")
tr <- trim_run(run, n_tr)
t_est <- Sys.time()
row <- estimate_all(tr, m2, input = inp)
wall_est <- as.numeric(difftime(Sys.time(), t_est, units = "secs"))
dv <- attr(row, "divergence")
smp <- run$samples
subset_cycles <- c(1, 2, 317, 318, n_main - 1, n_main)
sub <- smp[smp[, "cycle"] %in% subset_cycles, , drop = FALSE]
ev <- run$events
main_run <- list(
  n_cycles = n_main, n_transient = n_tr, d = 0.30, sd = 1.8e-4, seed = 1, input = inp$values,
  y0 = fp$y0, fell = run$fell, n_cycles_done = run$n_cycles_done,
  events = list(t_start = ev$t_start, t_end = ev$t_end, duration = ev$duration, u = ev$u,
                leg = ev$leg, theta_post = ev$theta_post, theta_dot_post = ev$theta_dot_post,
                phi_post = ev$phi_post, phi_dot_post = ev$phi_dot_post),
  samples_n = nrow(smp),
  # number of points of the resampling grid of the kept steps (grid_count(), walkbench 0.7.0)
  n_grid = grid_count(nrow(tr$events), m2$estimation_settings$resample$n_per, 2L),
  samples_subset = list(cycles = subset_cycles, t = sub[, "t"], thG = sub[, "thG"],
                        thGd = sub[, "thGd"], phG = sub[, "phG"], phGd = sub[, "phGd"],
                        cycle = sub[, "cycle"]),
  outcomes = as.list(row[1, outcome_names]),
  divergence = list(mld = dv$mld, time = dv$time, n_horizon = dv$n_horizon,
                    theiler = dv$theiler, n_pairs = dv$n_pairs),
  wall_time_s = list(simulate_run = wall_sim, estimate_all = wall_est))

# --- Reference at tight tolerances: same functions of M2 with lsodar rtol 1e-13 ---------------
parms <- m2$params[c("gamma", "k", "r_f")]
tight_rtol <- 1e-13
tight_atol <- 1e-15
cm_tight <- function(x, u) {
  st <- m2_integrate_step(x, parms, u, dt = 1, t_min = m2$params$t_min, rtol = tight_rtol,
                          atol = tight_atol)
  if (st$fell) stop("fall in the tight cycle map")
  list(x_next = m2_impact(st$y_end, r_f), duration = st$t_end)
}
xt <- fp$x_star
for (i in seq_len(30)) {
  f0 <- cm_tight(xt, 0)$x_next - xt
  if (sqrt(sum(f0^2)) < 1e-13) break
  jac <- matrix(0, 4, 4)
  for (j in 1:4) {
    xp <- xt
    xp[j] <- xp[j] + 1e-7
    jac[, j] <- (cm_tight(xp, 0)$x_next - xp - f0) / 1e-7
  }
  xt <- xt - solve(jac, f0)
}
# the loop repeats m2_simulate_loop() for an open loop, with the samples of the global grid
y <- fp$y0
d_t <- ts_t <- te_t <- numeric(n_main)
post_t <- matrix(NA_real_, n_main, 4)
sub_t <- list()
t_glob <- 0
stance <- 1L
for (k in seq_len(n_main)) {
  post_t[k, ] <- y
  st <- m2_integrate_step(y, parms, inp$values[k], dt = m2$params$dt, t_offset = t_glob,
                          t_min = m2$params$t_min, rtol = tight_rtol, atol = tight_atol)
  if (st$fell) stop("the main run fell at the tight tolerances")
  if (k %in% subset_cycles) {
    sm <- st$samples
    sub_t[[length(sub_t) + 1L]] <- cbind(t = sm[, 1] + t_glob,
                                         m2_leg_coords(sm[, 2:5, drop = FALSE], stance))
  }
  d_t[k] <- st$t_end
  ts_t[k] <- t_glob
  te_t[k] <- t_glob + st$t_end
  t_glob <- t_glob + st$t_end
  y <- m2_impact(st$y_end, r_f)
  stance <- 3L - stance
}
sub_t <- do.call(rbind, sub_t)
tight_reference <- list(rtol = tight_rtol, atol = tight_atol, x_star = xt,
                        period = cm_tight(xt, 0)$duration,
                        main_run = list(duration = d_t, t_start = ts_t, t_end = te_t,
                                        post_by_row = as.vector(t(post_t)),
                                        samples_subset = list(t = sub_t[, "t"],
                                                              thG = sub_t[, "thG"],
                                                              thGd = sub_t[, "thGd"],
                                                              phG = sub_t[, "phG"],
                                                              phGd = sub_t[, "phGd"])))

# --- Short run: trajectory, bounds and resampled state, for the estimators ------------------
inp_s <- input_arfima(n_tr + 110L, d = 0.30, sd = 1.8e-4, seed = 2, n_transient = n_tr)
run_s <- simulate_run(m2, inp_s, n_cycles = n_tr + 110L)
if (run_s$fell) stop("the short run fell; choose another seed")
tr_s <- trim_run(run_s, n_tr)
st_m2 <- settings_m2()
rs <- resample_states(tr_s, st_m2$state, "global_by_cycle", st_m2$resample$n_per, 2L,
                      coords = st_m2$model_coordinates)
dv_s <- estimate_divergence(tr_s, st_m2, 2L)
alt <- st_m2
alt$window_s <- c(0, 1)
alt$window_l <- c(3, 8)
dv_alt <- estimate_divergence(tr_s, alt, 2L)
div_out <- function(d) list(mld = d$mld, time = d$time, lambda_s = d$lambda_s,
                            lambda_l = d$lambda_l, lambda_s_per_step = d$lambda_s_per_step,
                            lambda_l_per_step = d$lambda_l_per_step, n_pairs = d$n_pairs,
                            n_horizon = d$n_horizon, theiler = d$theiler)
ss <- tr_s$samples
short_run <- list(
  n_cycles = n_tr + 110L, n_transient = n_tr, d = 0.30, sd = 1.8e-4, seed = 2,
  input = inp_s$values, dt = tr_s$dt,
  events = list(t_start = tr_s$events$t_start, t_end = tr_s$events$t_end,
                duration = tr_s$events$duration, leg = tr_s$events$leg),
  n_samples = nrow(ss), samples_t = ss[, "t"], samples_cycle = ss[, "cycle"],
  samples_by_row = as.vector(t(unname(as.matrix(ss[, c("thG", "thGd", "phG", "phGd")])))),
  bounds_start_by_row = as.vector(vapply(tr_s$bounds, function(b) as.numeric(b$start[1, ]), numeric(4))),
  bounds_end_by_row = as.vector(vapply(tr_s$bounds, function(b) as.numeric(b$end[1, ]), numeric(4))),
  resampled = list(n = nrow(rs$X), dt = rs$dt, X_by_row = as.vector(t(rs$X))),
  divergence_primary = c(div_out(dv_s), list(neighbors = dv_s$neighbors)),
  settings_alternative = list(window_s = alt$window_s, window_l = alt$window_l),
  divergence_alternative = div_out(dv_alt))

# --- A run that falls: first seed from 2001 whose run falls after step 40 -------------------
fall_run <- NULL
for (s in 2001:2200) {
  inp_f <- input_arfima(n_main, d = 0.45, sd = 4.5e-4, seed = s, n_transient = n_tr)
  r <- simulate_run(m2, inp_f, n_cycles = n_main)
  if (r$fell && r$fall_cycle > 40) {
    fall_run <- list(n_cycles = n_main, d = 0.45, sd = 4.5e-4, seed = s, input = inp_f$values,
                     fell = r$fell, fall_cycle = r$fall_cycle, n_cycles_done = r$n_cycles_done,
                     durations = r$events$duration)
    break
  }
}
if (is.null(fall_run)) stop("no run fell between seeds 2001 and 2200")

# --- Write ------------------------------------------------------------------------------------
meta$wall_time_s <- as.numeric(difftime(Sys.time(), t_start, units = "secs"))
vectors <- list(meta = meta, start_config = start_config, fixed_point = fixed_point,
                linearization = linearization, main_run = main_run,
                tight_reference = tight_reference, short_run = short_run, fall_run = fall_run)
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
jsonlite::write_json(vectors, out_file, auto_unbox = TRUE, digits = I(17), na = "null",
                     null = "null", pretty = FALSE)
cat(sprintf("written: %s (%.1f MB) in %.0f s\n", out_file, file.size(out_file) / 1e6,
            meta$wall_time_s))
cat(sprintf("M2: period %.6f, max |eig| %.4f, transient %d; main run lambda_s %.5f lambda_l %.5f; fall run seed %d at step %d\n",
            fp$period, max(Mod(fp$eig)), n_tr, row$lambda_s, row$lambda_l, fall_run$seed,
            fall_run$fall_cycle))
