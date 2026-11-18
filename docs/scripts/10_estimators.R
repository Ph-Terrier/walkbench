# Title: Worked example of the chapter "Estimators" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session of SIM-ACI (Claude Fable 5.2), without a code-generation
#   model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-13
# Description: applies the estimators of walkbench 0.6.2 to one run of model M1 and to one
#   run of model M2 with its default arguments: divergence exponents with the settings of each
#   model, Floquet multipliers against the reference stride of an unperturbed run, kinematic
#   variability, scaling exponents of the DFA and the summary row of estimate_all(). Each
#   model receives one uncorrelated input (independent uniform values) at one amplitude. A
#   series generated outside a model shows the two configurations of the DFA. The example is
#   a demonstration computed for the guide; it compares no outcome between inputs. Its seeds
#   lie in the block of the guide (90,000,000 to 92,099,999) and use the cells 100 to 102:
#   90,010,001 (input of M1), 90,010,101 (input of M2) and 90,010,201 (series of the DFA
#   demonstration). The blocks whose label starts with "hidden:" write the signatures of the
#   functions, register the numbers quoted in the chapter, check the statements of the text
#   and draw its two figures; the other blocks are printed in the chapter with their output.
#   The hidden blocks call two internal functions of the package, resample_states() and
#   m1_walker_env(), to check what the text says about the resampling and about the DFA of
#   the pinned walker file.
# Dependencies: walkbench 0.6.2 (loaded by the driver), stats, graphics, grDevices
# Input: none
# Output: generated/10_estimators/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_divergence.pdf, fig_floquet.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/10_estimators.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
for (f in c("settings_m2", "estimate_divergence", "estimate_floquet", "reference_xstar",
            "estimate_variability", "dfa_settings", "estimate_dfa", "estimate_all")) {
  guide_signature(f)
}
col <- guide_colors()
# counts above 9,999 are printed with a thousands separator
big <- function(x) format(x, big.mark = ",", trim = TRUE)
message_of <- function(expr) tryCatch({ expr; "no error" }, error = conditionMessage)

## ---- hidden: numbers-settings ----
# Values of the fields of the three lists of estimation settings, read from the objects and
# written as LaTeX text for the table of the chapter (underscores escaped)
settings_cell <- function(st, field, sub = NULL) {
  absent <- "\\textrm{(absent)}"
  if (!field %in% names(st)) return(absent)
  v <- st[[field]]
  if (!is.null(sub)) {
    if (!sub %in% names(v)) return(absent)
    v <- v[[sub]]
  }
  if (is.null(v)) return("NULL")
  s <- if (is.list(v)) {
    paste("list of", paste(names(v), collapse = ", "))
  } else if (is.character(v)) {
    paste0("\"", v, "\"", collapse = ", ")
  } else {
    paste(as.character(v), collapse = ", ")
  }
  gsub("_", "\\\\_", s)
}
settings_lists <- list(m1 = model_m1()$estimation_settings, m2 = settings_m2(),
                       m2c = settings_m2("continuous"))
settings_rows <- list(c("state", NA), c("coordinates", NA), c("model_coordinates", NA),
                      c("embedding", NA), c("resample", "method"), c("resample", "n_per"),
                      c("resample", "per"), c("window_unit", NA), c("theiler", NA),
                      c("horizon", NA), c("window_s", NA), c("window_l", NA), c("slope", NA),
                      c("smoothing", NA), c("pairs", NA))
for (id in names(settings_lists)) {
  for (r in settings_rows) {
    sub <- if (is.na(r[2])) NULL else r[2]
    key <- paste(c("set", id, r[1], sub), collapse = "_")
    guide_number(key, settings_cell(settings_lists[[id]], r[1], sub), "%s",
                 paste0("field ", paste(c(r[1], sub), collapse = "$"),
                        " of the estimation settings ", id, ", as LaTeX text"))
  }
}
known <- unique(unlist(lapply(settings_lists, names)))
if (!setequal(known, unique(vapply(settings_rows, `[`, "", 1)))) {
  stop("the table of the settings does not list every field")
}

## ---- m1-divergence ----
m1 <- model_m1()
n_transient <- transient_cycles(m1)
n_keep <- 300L
n_total <- n_transient + n_keep
u <- input_iid_uniform(n_total, rho = 0.004, seed = 90010001)
run <- trim_run(simulate_run(m1, u, n_cycles = n_total), n_transient)
st1 <- m1$estimation_settings
dv <- estimate_divergence(run, st1)
dv$unit
c(samples = nrow(run$samples), n_horizon = dv$n_horizon,
  theiler = dv$theiler, n_pairs = dv$n_pairs)
round(c(lambda_s = dv$lambda_s, lambda_l = dv$lambda_l), 4)
round(c(lambda_s_per_step = dv$lambda_s_per_step,
        lambda_l_per_step = dv$lambda_l_per_step), 4)

## ---- hidden: numbers-m1-divergence ----
if (run$fell) stop("the run of M1 was expected to complete")
if (!identical(names(dv), c("mld", "mld_smooth", "slopes", "time", "lambda_s", "lambda_l",
                            "n_pairs", "neighbors", "n_horizon", "theiler",
                            "lambda_s_per_step", "lambda_l_per_step", "unit"))) {
  stop("the list of estimate_divergence() was expected to have the 13 elements of the chapter")
}
if (!identical(colnames(run$samples), c("t", "thG", "thGd", "phG", "phGd", "cycle"))) {
  stop("the samples were expected to hold the time, the four coordinates and the cycle")
}
t_mean <- mean(run$events$duration)
guide_number("n_transient", n_transient, "%d", "transient cycles of M1")
guide_number("n_keep", n_keep, "%d", "kept cycles of the example")
guide_number("n_strides", n_keep %/% 2L, "%d", "strides of the kept cycles")
guide_number("rho_m1", 1000 * u$params$rho, "%.0f", "rho of the uniform input of M1, in 1e-3 rad")
guide_number("m1_samples", big(nrow(run$samples)), "%s", "samples of the trimmed run of M1")
guide_number("m1_dt", run$dt, "%.2f", "sampling interval of M1, in model time units")
guide_number("m1_t_mean", t_mean, "%.3f", "mean step duration of the kept cycles of M1, in model time units")
guide_number("m1_n_horizon", dv$n_horizon, "%.0f", "horizon of M1, in samples")
guide_number("m1_theiler", dv$theiler, "%.0f", "exclusion window of M1, in samples")
guide_number("m1_n_pairs", big(dv$n_pairs), "%s", "pairs of M1 that span the horizon")
guide_number("m1_lambda_s", dv$lambda_s, "%.3f", "short-term exponent of M1, per unit of model time")
guide_number("m1_lambda_l", dv$lambda_l, "%.4f", "long-term exponent of M1, per unit of model time")
guide_number("m1_lambda_s_step", dv$lambda_s_per_step, "%.2f", "short-term exponent of M1, per step")
guide_number("m1_lambda_l_step", dv$lambda_l_per_step, "%.3f", "long-term exponent of M1, per step")
in_s <- which(!is.na(dv$slopes) & dv$time >= st1$window_s[1] * t_mean &
                dv$time <= st1$window_s[2] * t_mean)
in_l <- which(!is.na(dv$slopes) & dv$time >= st1$window_l[1] * t_mean &
                dv$time <= st1$window_l[2] * t_mean)
if (!isTRUE(all.equal(mean(dv$slopes[in_s]), dv$lambda_s)) ||
    !isTRUE(all.equal(mean(dv$slopes[in_l]), dv$lambda_l))) {
  stop("the exponents of M1 were expected to be the means of the local slopes of the windows")
}
guide_number("m1_n_slopes_s", length(in_s), "%d", "local slopes averaged in the short-term window of M1")
guide_number("m1_n_slopes_l", length(in_l), "%d", "local slopes averaged in the long-term window of M1")
guide_number("m1_first_slope", min(which(!is.na(dv$slopes))) - 1L, "%d", "lag of the first local slope of M1, in samples")
guide_number("m1_window_s_samples", floor(st1$window_s[2] * t_mean / run$dt), "%.0f", "last lag of the short-term window of M1, in samples")
if (!identical(sum(is.na(dv$mld_smooth)), 6L)) stop("the smoothed curve was expected to lack 6 values")
if (!isTRUE(all.equal(dv$lambda_s_per_step, dv$lambda_s * t_mean)) ||
    !isTRUE(all.equal(dv$lambda_l_per_step, dv$lambda_l * t_mean))) {
  stop("the conversion per step of M1 was expected to use the mean step duration")
}

## ---- m1-floquet ----
zero <- simaci_input(numeric(n_total), source = "zero", redrawable = FALSE)
run_ref <- trim_run(simulate_run(m1, zero, n_cycles = n_total), n_transient)
xstar <- reference_xstar(run_ref, m1)
fm <- estimate_floquet(run, m1, xstar)
c(dim(xstar), length(fm))
round(fm[c(26, 51, 76)], 3)
round(range(fm), 3)

## ---- hidden: numbers-m1-floquet ----
if (!identical(estimate_floquet(run, m1, xstar, sections = c(26, 51, 76)), fm[c(26, 51, 76)])) {
  stop("the argument sections was expected to select sections of the whole profile")
}
guide_number("fm25", fm[26], "%.2f", "largest Floquet multiplier of M1 at 25% of the stride")
guide_number("fm50", fm[51], "%.2f", "largest Floquet multiplier of M1 at 50% of the stride")
guide_number("fm75", fm[76], "%.2f", "largest Floquet multiplier of M1 at 75% of the stride")
guide_number("fm_min", min(fm), "%.2f", "smallest value of the Floquet profile of M1")
guide_number("fm_max", max(fm), "%.2f", "largest value of the Floquet profile of M1")
arr_ref <- m1$normalize_cycles(run_ref)
if (!identical(dim(arr_ref), c(150L, 101L, 4L))) stop("150 normalized strides of 101 sections and 4 coordinates were expected")
sd_ref <- apply(arr_ref, c(2, 3), sd)
if (!identical(names(which.max(apply(sd_ref, 2, max))), "thG")) stop("the largest SD of the unperturbed run was expected for thG")
guide_number("ref_sd_max", 1e5 * max(sd_ref), "%.1f",
             "largest SD of thG across the normalized strides of the unperturbed run of M1, in 1e-5 rad")
# the same quantity with a sampling interval ten times shorter (100 kept cycles in both runs)
sd_across <- function(dt) {
  m <- model_m1(dt = dt)
  r <- trim_run(simulate_run(m, simaci_input(numeric(n_transient + 100L), source = "zero",
                                             redrawable = FALSE),
                             n_cycles = n_transient + 100L), n_transient)
  max(apply(m$normalize_cycles(r), c(2, 3), sd))
}
sd_ratio <- sd_across(0.03) / sd_across(0.003)
if (sd_ratio < 80 || sd_ratio > 125) stop("the SD across the strides was expected to scale with the square of the sampling interval")
guide_number("ref_sd_ratio", sd_ratio, "%.0f",
             "ratio of the largest SD across the normalized strides of unperturbed runs of M1 sampled at 0.03 and at 0.003 time units")
# after a foot strike, phG = -2 thG in M1: in the reference stride and in every stride of the run
arr_run <- m1$normalize_cycles(run)
if (!isTRUE(all.equal(unname(xstar[c(1, 51), "phG"]), unname(-2 * xstar[c(1, 51), "thG"]))) ||
    max(abs(arr_run[, c(1, 51), "phG"] + 2 * arr_run[, c(1, 51), "thG"])) > 1e-12) {
  stop("phG = -2 thG was expected at the two post-strike sections of the strides of M1")
}

## ---- hidden: figure-floquet ----
guide_figure("floquet", width_cm = 15.5, height_cm = 5.0, draw = function() {
  par(mar = c(3.3, 4.0, 0.5, 0.7), mgp = c(2.2, 0.6, 0), tcl = -0.3, las = 1)
  y_top <- ceiling(10 * max(fm) + 1) / 10
  plot(0:100, fm, type = "n", axes = FALSE, xlab = "Poincaré section (% of the stride)",
       ylab = "Largest modulus", xlim = c(0, 100), ylim = c(0, y_top), yaxs = "i")
  segments(c(0, 50, 100), 0, c(0, 50, 100), y_top, col = col[["gray"]], lty = 3)
  lines(0:100, fm, col = col[["blue"]], lwd = 1.3)
  at <- c(25, 50, 75)
  points(at, fm[at + 1L], pch = 16, col = col[["blue"]])
  text(at, fm[at + 1L], c("fm25", "fm50", "fm75"), pos = c(1, 4, 1), cex = 0.9, offset = 0.5)
  text(50, 0.06, "foot strike", pos = 4, cex = 0.9, offset = 0.3)
  axis(1, at = seq(0, 100, by = 25), col = col[["gray"]])
  axis(2, at = seq(0, y_top, by = 0.2), col = col[["gray"]])
})

## ---- m1-summary ----
v <- estimate_variability(run, m1)
row <- estimate_all(run, m1, xstar = xstar, cell = 100, replicate = 1)
row[, 1:6]
signif(row[, 7:13], 3)
round(row[, 14:17], 3)
round(row[, 18:22], 4)
round(row[, 23:27], 3)
identical(unname(v$msd), unlist(row[, 10:13], use.names = FALSE))
identical(estimate_dfa(run$events$duration), row$alpha_duration)
identical(attr(row, "divergence"), dv)

## ---- hidden: numbers-m1-summary ----
if (!identical(names(v), c("msd", "mean_duration", "sd_duration", "cv_duration"))) {
  stop("the list of estimate_variability() was expected to have four elements")
}
guide_number("msd_thG", 1000 * v$msd[["thG"]], "%.2f", "MeanSD of thG of M1, in 1e-3 rad")
guide_number("msd_phG", 1000 * v$msd[["phG"]], "%.2f", "MeanSD of phG of M1, in 1e-3 rad")
guide_number("m1_cv_pct", 100 * v$cv_duration, "%.2f", "coefficient of variation of the step durations of M1, in percent")
guide_number("m1_alpha_duration", estimate_dfa(run$events$duration), "%.2f", "DFA alpha of the step durations of M1")
kept <- n_transient + seq_len(n_keep)
strides <- colSums(matrix(run$events$duration, nrow = 2))
checks <- c(
  identical(names(row)[1:6], c("model", "injection", "cell", "replicate", "n_redraws", "fell")),
  identical(unname(unlist(row[, c("fm25", "fm50", "fm75")])), fm[c(26, 51, 76)]),
  identical(row$fm_max, max(fm)),
  identical(unname(unlist(row[, c("mean_duration", "sd_duration", "cv_duration")])),
            unname(unlist(v[2:4]))),
  identical(row$alpha_input, estimate_dfa(u$values[kept])),
  identical(row$alpha_command, row$alpha_input),
  identical(row$alpha_stride_duration, estimate_dfa(strides)),
  is.na(row$alpha_asynchrony), is.na(row$n_redraws),
  identical(row$lambda_l_per_step, dv$lambda_l_per_step), identical(row$n_pairs, dv$n_pairs))
if (!all(checks)) stop("a column of the summary row differs from the value of its estimator: ",
                       paste(which(!checks), collapse = ", "))
guide_number("n_columns", ncol(row), "%d", "columns of the summary row of M1")
guide_number("n_outcomes", ncol(row) - 6L, "%d", "outcome columns of the summary row of M1")
guide_number("m1_alpha_input", row$alpha_input, "%.2f", "DFA alpha of the kept part of the input of M1")
guide_number("m1_alpha_stride", row$alpha_stride_duration, "%.2f", "DFA alpha of the stride durations of M1")
# without a reference stride and with 40 kept cycles: empty columns
row_no_ref <- estimate_all(trim_run(simulate_run(m1, u, n_cycles = n_transient + 40L),
                                    n_transient), m1)
if (!all(is.na(unlist(row_no_ref[, c("fm25", "fm50", "fm75", "fm_max")])))) {
  stop("without a reference stride, the four columns of the Floquet multipliers were expected to be empty")
}
if (!is.na(row_no_ref$alpha_stride_duration) || !is.na(row_no_ref$alpha_duration)) {
  stop("40 kept cycles were expected to give no scaling exponent of the durations")
}
# a run that fell: same input, initial state with its two angular velocities increased by 30%
y0_far <- m1$fixed_point()$y0 * c(1, 1.3, 1, 1.3)
run_fall <- simulate_run(m1, u, n_cycles = n_total, y0 = y0_far)
row_fall <- estimate_all(run_fall, m1, cell = 100, replicate = 2)
if (!run_fall$fell || !identical(names(row_fall), names(row)) ||
    !all(is.na(unlist(row_fall[, 7:ncol(row_fall)]))) || !is.null(attr(row_fall, "divergence"))) {
  stop("the displaced run was expected to fall and to give the same columns with empty outcomes, without the attribute")
}
if (!identical(message_of(trim_run(run_fall, n_transient)), "a run that fell is not trimmed")) {
  stop("trim_run() was expected to stop on a run that fell")
}
guide_number("fall_cycle", run_fall$fall_cycle, "%d", "cycle of the fall of the displaced run of M1")
guide_number("fall_factor_pct", 30, "%d", "increase of the two angular velocities of the displaced initial state, in percent")

## ---- m2-divergence ----
m2 <- model_m2()
st2 <- settings_m2()
identical(st2, m2$estimation_settings)
n_transient2 <- transient_cycles(m2)
n_total2 <- n_transient2 + n_keep
u2 <- input_iid_uniform(n_total2, rho = 1e-4, seed = 90010101)
u2$value_unit <- "leg length"
run2 <- trim_run(simulate_run(m2, u2, n_cycles = n_total2), n_transient2)
dv2 <- estimate_divergence(run2, st2)
dv2$unit
c(points = length(dv2$neighbors), n_horizon = dv2$n_horizon,
  theiler = dv2$theiler, n_pairs = dv2$n_pairs)
round(c(lambda_s = dv2$lambda_s, lambda_l = dv2$lambda_l), 4)
round(c(lambda_s_per_step = dv2$lambda_s_per_step,
        lambda_l_per_step = dv2$lambda_l_per_step), 4)

## ---- hidden: numbers-m2 ----
if (run2$fell) stop("the run of M2 was expected to complete")
guide_number("n_transient2", n_transient2, "%d", "transient cycles of M2 with its default arguments")
guide_number("rho_m2", 1e4 * u2$params$rho, "%.0f", "rho of the uniform input of M2, in 1e-4 leg length")
guide_number("m2_points", big(length(dv2$neighbors)), "%s", "points of the resampled trajectory of M2")
guide_number("m2_n_horizon", big(dv2$n_horizon), "%s", "horizon of M2, in samples")
guide_number("m2_theiler", dv2$theiler, "%.0f", "exclusion window of M2, in samples")
guide_number("m2_n_pairs", big(dv2$n_pairs), "%s", "pairs of M2 that span the horizon")
guide_number("m2_lambda_s", dv2$lambda_s, "%.2f", "short-term exponent of M2, per stride")
guide_number("m2_lambda_l", dv2$lambda_l, "%.4f", "long-term exponent of M2, per stride")
guide_number("m2_lambda_s_step", dv2$lambda_s_per_step, "%.2f", "short-term exponent of M2, per step")
guide_number("m2_lambda_l_step", dv2$lambda_l_per_step, "%.4f", "long-term exponent of M2, per step")
if (!isTRUE(all.equal(dv2$lambda_s_per_step, dv2$lambda_s / 2)) ||
    !isTRUE(all.equal(dv2$lambda_l_per_step, dv2$lambda_l / 2)) || !all(is.na(dv2$slopes)) ||
    !identical(dv2$mld, dv2$mld_smooth)) {
  stop("with the settings of M2, the exponent per step was expected to be half the exponent per stride, without smoothing and without local slopes")
}
# least-squares lines of the two windows, for the figure and as a check of the two exponents
fit_window <- function(d, w) {
  sel <- d$time >= w[1] & d$time <= w[2]
  cf <- lm.fit(cbind(1, d$time[sel]), d$mld[sel])$coefficients
  list(t = range(d$time[sel]), cf = unname(cf), n = sum(sel))
}
fit_s <- fit_window(dv2, st2$window_s)
fit_l <- fit_window(dv2, st2$window_l)
if (!isTRUE(all.equal(fit_s$cf[2], dv2$lambda_s)) || !isTRUE(all.equal(fit_l$cf[2], dv2$lambda_l))) {
  stop("the exponents of M2 were expected to be the least-squares slopes of the two windows")
}
guide_number("m2_n_fit_s", fit_s$n, "%d", "points of the least-squares line of the short-term window of M2")
guide_number("m2_n_fit_l", fit_l$n, "%d", "points of the least-squares line of the long-term window of M2")
# resampled trajectory of M2, as estimate_divergence() builds it (internal function)
rs2 <- resample_states(run2, st2$state, st2$resample$method, st2$resample$n_per,
                       m2$cycles_per_stride, coords = st2$model_coordinates)
if (!identical(nrow(rs2$X), length(dv2$neighbors))) stop("the resampled trajectory was expected to have one row per point")
guide_number("m2_dt", rs2$dt, "%.2f", "sampling interval of the resampled trajectory of M2, in strides")
guide_number("m2_theta_jump", max(abs(diff(rs2$X[, 1]))), "%.2f", "largest difference of theta between two successive points of the resampled trajectory of M2, in rad")

## ---- hidden: figure-divergence ----
guide_figure("divergence", width_cm = 15.5, height_cm = 5.8, draw = function() {
  t_steps <- dv$time / t_mean
  par(mfrow = c(1, 3), mar = c(3.3, 3.5, 1.9, 0.5), mgp = c(2.2, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  band <- function(w, yl) rect(w[1], yl[1], w[2], yl[2], col = col[["light"]], border = NA)
  frame <- function(x, y, xlim, yl, xlab, ylab) {
    plot(x, y, type = "n", axes = FALSE, xlab = xlab, ylab = ylab, xlim = xlim, ylim = yl,
         xaxs = "i", yaxs = "i")
  }
  tag <- function(s) title(main = s, adj = 0, font.main = 1, cex.main = 1, line = 0.6)
  # (a) M1, whole horizon
  yl <- range(dv$mld) + c(-0.05, 0.12) * diff(range(dv$mld))
  frame(t_steps, dv$mld, c(0, 4), yl, "Time (steps)", "Mean ln divergence")
  band(st1$window_s, yl)
  band(st1$window_l, yl)
  lines(t_steps, dv$mld, col = col[["blue"]], lwd = 1.3)
  text(0.32, yl[1] + 0.07 * diff(yl), expression(lambda[S]), adj = 0, cex = 0.95)
  text(3.5, yl[1] + 0.07 * diff(yl), expression(lambda[L]), adj = 0.5, cex = 0.95)
  axis(1, at = 0:4, col = col[["gray"]])
  axis(2, col = col[["gray"]])
  tag("(a) M1, whole horizon")
  # (b) M1, first 0.3 step
  sel <- which(t_steps <= 0.3)
  yb <- range(dv$mld[sel]) + c(-0.06, 0.06) * diff(range(dv$mld[sel]))
  frame(t_steps[sel], dv$mld[sel], c(-0.008, 0.3), yb, "Time (steps)", "")
  band(st1$window_s, yb)
  lines(t_steps[sel], dv$mld_smooth[sel], col = col[["vermilion"]], lwd = 1.3)
  points(t_steps[sel], dv$mld[sel], pch = 16, cex = 0.5, col = col[["blue"]])
  text(0.05, yb[1] + 0.07 * diff(yb), expression(lambda[S]), adj = 0.5, cex = 0.95)
  axis(1, at = c(0, 0.1, 0.2, 0.3), col = col[["gray"]])
  axis(2, col = col[["gray"]])
  legend("topleft", legend = c("curve", "smoothed"), pch = c(16, NA), lty = c(NA, 1),
         lwd = c(NA, 1.3), pt.cex = 0.5, col = c(col[["blue"]], col[["vermilion"]]),
         bty = "n", cex = 0.9, inset = c(0.0, 0.0))
  tag("(b) M1, first 0.3 step")
  # (c) M2, whole horizon, with the least-squares lines of the two windows
  yc <- range(dv2$mld) + c(-0.05, 0.12) * diff(range(dv2$mld))
  frame(dv2$time, dv2$mld, c(0, 10), yc, "Time (strides)", "")
  band(st2$window_s, yc)
  band(st2$window_l, yc)
  lines(dv2$time, dv2$mld, col = col[["blue"]], lwd = 1.3)
  for (f in list(fit_s, fit_l)) {
    lines(f$t, f$cf[1] + f$cf[2] * f$t, col = col[["vermilion"]], lwd = 1.6)
  }
  text(0.9, yc[1] + 0.07 * diff(yc), expression(lambda[S]), adj = 0, cex = 0.95)
  text(7, yc[1] + 0.07 * diff(yc), expression(lambda[L]), adj = 0.5, cex = 0.95)
  axis(1, at = seq(0, 10, by = 2), col = col[["gray"]])
  axis(2, col = col[["gray"]])
  legend("right", legend = c("curve", "fitted lines"), lty = 1, lwd = c(1.3, 1.6),
         col = c(col[["blue"]], col[["vermilion"]]), bty = "n", cex = 0.9, inset = 0.02)
  tag("(c) M2, whole horizon")
})

## ---- dfa ----
x <- input_arfima(1200, d = 0.3, sd = 1, seed = 90010201)$values
plan <- dfa_settings()
walker <- dfa_settings("feasibility")
round(c(terrier2026 = estimate_dfa(x, plan),
        feasibility = estimate_dfa(x, walker)), 3)
short <- x[1:90]
round(c(terrier2026 = estimate_dfa(short, plan),
        feasibility = estimate_dfa(short, walker)), 3)
estimate_dfa(rep(1, 200))

## ---- hidden: numbers-dfa ----
if (!is.na(estimate_dfa(c(x[1:199], NA))) || !is.na(estimate_dfa(c(x[1:199], Inf)))) {
  stop("a series with a value that is not finite was expected to give NA")
}
guide_number("dfa_n", big(length(x)), "%s", "length of the series of the DFA demonstration")
guide_number("dfa_d", 0.3, "%.1f", "memory parameter of the series of the DFA demonstration")
guide_number("dfa_alpha_plan", estimate_dfa(x, plan), "%.2f", "DFA alpha of the series, configuration terrier2026")
guide_number("dfa_alpha_walker", estimate_dfa(x, walker), "%.2f", "DFA alpha of the series, configuration feasibility")
guide_number("dfa_n_short", length(short), "%d", "length of the short series of the DFA demonstration")
guide_number("dfa_alpha_short_walker", estimate_dfa(short, walker), "%.2f", "DFA alpha of the short series, configuration feasibility")
# box sizes of the two configurations for the series of the demonstration, computed as
# dfa_terrier2026() of the package and dfa_alpha() of the pinned walker file compute them
sizes_plan <- function(n_x) {
  n_max <- floor(n_x * plan$n_max_ratio)
  s <- unique(round(plan$n_min * (n_max / plan$n_min)^(seq(0, 1, length.out = plan$n_sizes))))
  s[s >= plan$n_min & s <= n_max]
}
sizes_walker <- function(n_x) {
  unique(round(exp(seq(log(walker$n_min), log(floor(n_x * walker$n_max_ratio)),
                       length.out = walker$n_sizes))))
}
guide_number("dfa_sizes_plan_n", length(sizes_plan(length(x))), "%d", "box sizes of the configuration terrier2026 for the series")
guide_number("dfa_sizes_plan_max", max(sizes_plan(length(x))), "%d", "largest box of the configuration terrier2026 for the series")
guide_number("dfa_sizes_walker_n", length(sizes_walker(length(x))), "%d", "box sizes of the configuration feasibility for the series")
guide_number("dfa_sizes_walker_max", max(sizes_walker(length(x))), "%d", "largest box of the configuration feasibility for the series")
guide_number("dfa_sizes_plan_96", length(sizes_plan(96)), "%d", "box sizes of the configuration terrier2026 for 96 values")
if (!identical(range(sizes_plan(96)), c(16, 32))) stop("the boxes of 96 values were expected to run from 16 to 32 values")
if (!is.na(estimate_dfa(x[1:95], plan)) || is.na(estimate_dfa(x[1:96], plan))) {
  stop("the configuration terrier2026 was expected to need 96 values")
}
if (!all(is.na(vapply(16:19, function(n) estimate_dfa(x[seq_len(n)], walker), 1))) ||
    is.na(estimate_dfa(x[1:20], walker)) || !is.na(estimate_dfa(x[1:15], walker)) ||
    length(sizes_walker(20)) != 2L) {
  stop("the configuration feasibility was expected to return NA up to 19 values and a number from 20 values, with two box sizes")
}
if (!identical(estimate_dfa(x, walker), m1_walker_env()$dfa_alpha(x)$alpha)) {
  stop("the configuration feasibility was expected to call dfa_alpha() of the pinned walker file")
}
# a changed field of the default configuration changes the result; for the second
# configuration only min_length acts in estimate_dfa()
plan_10 <- plan
plan_10$n_min <- 10
walker_10 <- walker
walker_10$n_min <- 10
if (identical(estimate_dfa(x, plan_10), estimate_dfa(x, plan)) ||
    !identical(estimate_dfa(x, walker_10), estimate_dfa(x, walker))) {
  stop("the field n_min was expected to act for terrier2026 only")
}

## ---- hidden: checks-messages ----
# Messages of the table of errors of the chapter, produced on a short run of M2 (40 kept
# cycles); the message on a non-finite state after the resampling is not produced
run_short <- trim_run(simulate_run(m2, u2, n_cycles = n_transient2 + 40L), n_transient2)
change <- function(st, ...) utils::modifyList(st, list(...), keep.null = TRUE)
stc <- settings_m2("continuous")
no_maps <- st2
no_maps$model_coordinates$maps <- NULL
messages <- c(
  message_of(estimate_divergence(run_short, no_maps)),
  message_of(estimate_divergence(run_short, change(st2, resample = list(method = "none")))),
  message_of(estimate_divergence(run_short, change(st2, resample = list(method = "global")))),
  message_of(estimate_divergence(run_short, change(st2, resample = list(method = "per_cycle")))),
  message_of(estimate_divergence(run_short, change(st2, state = c("theta", "psi")))),
  message_of(estimate_divergence(run_short, change(stc, resample = list(method = "spline")))),
  message_of(estimate_divergence(run_short, change(stc, slope = "robust"))),
  message_of(estimate_divergence(run_short, change(stc, state = "thG",
                                                   embedding = list(m = 5, tau = 2000)))),
  message_of(estimate_floquet(run_short, m2, xstar, sections = 102)),
  message_of(estimate_floquet(run_short, m2, xstar[1:50, ], sections = 51)),
  message_of(estimate_dfa(x, list(method = "terrier2026"))),
  message_of(dfa_settings("other")))
expected <- c(
  "settings$model_coordinates must give names, sampled and maps",
  "the state variables of the model need a resampling",
  "the state variables of the model need the resampling \"global_by_cycle\"",
  "the state variables of the model need the resampling \"global_by_cycle\"",
  "unknown state variables of the model: psi",
  "unknown resampling method: spline",
  "unknown slope method: robust",
  "Time series too short for given embedding parameters",
  "subscript out of bounds",
  "subscript out of bounds",
  "dfa must come from dfa_settings()",
  "'arg' should be one of \"terrier2026\", \"feasibility\"")
if (!identical(messages, expected)) {
  stop("a message of the table of errors differs: ", paste(messages[messages != expected], collapse = " | "))
}

## ---- hidden: checks-options ----
# Statements of the chapter on the options that no model of version 0.6.2 uses, checked on
# the short run of M2 (40 kept cycles) in the continuous leg coordinates
dvc <- estimate_divergence(run_short, stc)
n_c <- length(dvc$neighbors)
# fixed set of pairs: the points left out receive the neighbor N + 1
stf <- stc
stf$pairs <- "fixed"
dvf <- estimate_divergence(run_short, stf)
left_out <- dvf$neighbors == n_c + 1L
if (!identical(dvf$n_pairs, dvc$n_pairs) || sum(!left_out) != dvf$n_pairs ||
    !identical(dvf$neighbors[!left_out], dvc$neighbors[!left_out])) {
  stop("the fixed set was expected to hold the pairs that span the horizon")
}
# delay embedding of one coordinate: a list with one element per coordinate
ste <- stc
ste$state <- "thG"
ste$embedding <- list(m = 5, tau = 10)
dve <- estimate_divergence(run_short, ste)
if (!identical(names(dve), "thG") || !identical(length(dve$thG$neighbors), n_c - 40L)) {
  stop("the embedding was expected to return one list per coordinate, with N - (m - 1) tau points")
}
msg_embed <- message_of(estimate_all(run_short, m2, settings = ste))
if (!identical(msg_embed, "arguments imply differing number of rows: 1, 0")) {
  stop("estimate_all() with an embedding was expected to stop in data.frame(): ", msg_embed)
}
# resampling "per_cycle": n_per points for each cycle
stp <- stc
stp$resample <- list(method = "per_cycle", n_per = 50, per = "stride")
dvp <- estimate_divergence(run_short, stp)
if (!identical(length(dvp$neighbors), 50L * run_short$n_cycles_done) ||
    !identical(dvp$unit, "per stride") || dvp$n_horizon != 1000) {
  stop("the resampling per cycle was expected to give 50 points per cycle and 100 per stride")
}
# boundary states read by position with "global" and "per_cycle", by name with
# "global_by_cycle": a coordinate taken alone differs from the same coordinate of the full
# state with the first two methods
alone <- function(method, n_per) {
  full <- resample_states(run_short, stc$state, method, n_per, 2)$X[, 3]
  one <- resample_states(run_short, "phG", method, n_per, 2)$X[, 1]
  sum(full != one)
}
if (alone("global", 100) == 0L || alone("per_cycle", 50) == 0L ||
    alone("global_by_cycle", 100) != 0L) {
  stop("the boundary states were expected to be read by position with the methods global and per_cycle")
}
# a run shorter than the horizon: no error, and no pair that spans the horizon
run_tiny <- trim_run(simulate_run(m1, u, n_cycles = n_transient + 6L), n_transient)
dvt <- estimate_divergence(run_tiny, st1)
if (dvt$n_pairs != 0L || !is.finite(dvt$lambda_s)) {
  stop("a run of 6 kept cycles of M1 was expected to return exponents with n_pairs = 0")
}
guide_number("tiny_cycles", run_tiny$n_cycles_done, "%d", "kept cycles of the run of M1 that is shorter than needed")
# values of smoothing and pairs that the function does not know: no error
sto <- stc
sto$smoothing <- "other"
sto$pairs <- "other"
if (!identical(estimate_divergence(run_short, sto)$lambda_l, dvc$lambda_l)) {
  stop("unknown values of smoothing and pairs were expected to act as \"none\" and \"shrinking\"")
}
# a reference stride of another model passes without an error
if (!is.numeric(estimate_floquet(run_short, m2, xstar, sections = 26))) {
  stop("a reference stride of M1 was expected to pass with a run of M2")
}
