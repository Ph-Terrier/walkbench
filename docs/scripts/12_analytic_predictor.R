# Title: Worked example of the chapter "Analytic predictor" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session of SIM-ACI (Claude Fable 5.2), without a code-generation
#   model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-13
# Description: demonstrates the twelve functions of the analytic predictor of walkbench 0.6.2.
#   The script computes the autocorrelations of a fractionally integrated series and of an
#   AR(1) series, the autocovariance of an uncorrelated input centered and rescaled over a
#   series of 320 values, and the sample autocovariance of one generated uncorrelated input.
#   It then computes the levels and the predicted long-term exponent of model M1 for an
#   uncorrelated input (d = 0) with a standard deviation of 0.002 rad and 300 kept steps, the
#   settings of the predictor for model M2 with its default arguments, the levels and the
#   predicted exponent of that configuration for an uncorrelated input, and the memory
#   parameter of a fractionally integrated series matched to one AR(1) coefficient. The
#   example is a demonstration computed for the guide. It runs no simulation of a model, it
#   predicts an exponent for uncorrelated inputs only, and it sets no predicted value beside
#   a simulated one. Its only seed, 90,012,001, lies in the block of the guide (cell 120).
#   The blocks whose label starts with "hidden:" write the signatures of the functions,
#   register the numbers quoted in the chapter, make the checks that the text relies on and
#   draw the figure; the other blocks are printed in the chapter with their output.
# Dependencies: walkbench 0.6.2 (loaded by the driver), stats, graphics, grDevices
# Input: none
# Output: generated/12_analytic_predictor/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_weights.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/12_analytic_predictor.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
# The chapter prints the signatures of each group of functions in one box. Each signature
# comes from guide_signature(), which reads it from the loaded package and writes one file
# per function; the lines of a group are then written to signatures/<group>.txt.
signature_group <- function(group, functions) {
  sig <- unlist(lapply(functions, guide_signature))
  writeLines(sig, file.path(guide_dir(), "signatures", paste0(group, ".txt")), useBytes = TRUE)
}
dir.create(file.path(guide_dir(), "signatures"), showWarnings = FALSE)
signature_group("group_autocovariance", c("fi_autocorrelation", "ar1_autocorrelation",
                                          "centered_autocovariance", "sample_autocovariance"))
signature_group("group_levels", c("spinal_d_grid", "predict_spinal_levels",
                                  "spinal_lambda_l_m1", "spinal_lambda_l_lsq"))
signature_group("group_settings_m2", c("spinal_n_past", "spinal_settings_m2"))
signature_group("group_matching", c("window_slope_half_log_v", "match_fi_to_ar1"))
if (length(formals(spinal_d_grid)) != 0L) stop("spinal_d_grid() was expected to have no argument")
col <- guide_colors()
msg_of <- function(expr) tryCatch({ expr; "no error" }, error = conditionMessage)

## ---- autocorrelation ----
rho_fi <- fi_autocorrelation(d = 0.3, n_lags = 31)
rho_ar <- ar1_autocorrelation(phi = 0.5, n_lags = 31)
lags <- c(0, 1, 2, 5, 10, 30)
tab <- rbind(fi = rho_fi[lags + 1], ar1 = rho_ar[lags + 1])
colnames(tab) <- paste("lag", lags)
round(tab, 4)

## ---- hidden: numbers-autocorrelation ----
if (!isTRUE(all.equal(rho_fi[2], 0.3 / 0.7))) stop("rho_1 of the FI series was expected to equal d / (1 - d)")
if (!identical(fi_autocorrelation(0, 5), c(1, 0, 0, 0, 0))) stop("d = 0 was expected to give zero autocorrelations beyond lag 0")
guide_number("fi_d", 0.3, "%.1f", "memory parameter of the fractionally integrated series of the first block")
guide_number("ar_phi", 0.5, "%.1f", "coefficient of the AR(1) series of the example")
guide_number("fi_rho10", rho_fi[11], "%.4f", "autocorrelation at lag 10 of the fractionally integrated series, d = 0.3")
guide_number("ar_rho10", rho_ar[11], "%.4f", "autocorrelation at lag 10 of the AR(1) series, phi = 0.5")

## ---- autocovariance ----
m1 <- model_m1()
n_keep <- 300L
n_total <- transient_cycles(m1) + n_keep
sd_u <- 0.002                    # standard deviation of the input (rad)
# lags of the default call: n_past + max(k_levels) + max(d_grid)
n_lags <- 60 + 4 + max(spinal_d_grid())
rho_0 <- fi_autocorrelation(d = 0, n_lags = n_lags)
cg <- centered_autocovariance(rho_0, n_center = n_total, sd = sd_u)
cg$vbar
cg$gam[1:3] / sd_u^2
rho_p <- fi_autocorrelation(d = 0.3, n_lags = n_lags)
centered_autocovariance(rho_p, n_center = n_total, sd = sd_u)$vbar
u <- input_arfima(n_total, d = 0, sd = sd_u, seed = 90012001)
round(sample_autocovariance(u$values, n_lags = 5) / sd_u^2, 4)

## ---- hidden: numbers-autocovariance ----
n_transient <- transient_cycles(m1)
vbar_p <- centered_autocovariance(rho_p, n_center = n_total, sd = sd_u)$vbar
if (!isTRUE(all.equal(cg$vbar, 1 / n_total))) stop("vbar of an uncorrelated series was expected to equal 1 / n_center")
if (!isTRUE(all.equal(cg$gam[2] / sd_u^2, -1 / (n_total - 1)))) stop("the centered autocovariance of an uncorrelated series was expected to equal -sd^2 / (n - 1) beyond lag 0")
if (length(cg$gam) != n_lags) stop("gam was expected to have the lags of rho")
c_hat <- sample_autocovariance(u$values, n_lags = 5)
acf_r <- as.numeric(acf(u$values, lag.max = 4, type = "covariance", plot = FALSE)$acf)
if (!isTRUE(all.equal(c_hat, acf_r))) stop("sample_autocovariance() was expected to equal acf(type = \"covariance\")")
if (!isTRUE(all.equal(c_hat[1], sd_u^2 * (n_total - 1) / n_total))) stop("the sample autocovariance at lag 0 was expected to use the divisor n")
guide_number("n_transient", n_transient, "%d", "transient cycles of M1, transient_cycles(model_m1())")
guide_number("n_keep", n_keep, "%d", "kept steps of the example of M1")
guide_number("n_total", n_total, "%d", "values of the input of the example of M1, transient cycles included")
guide_number("sd_u", 1000 * sd_u, "%.0f", "standard deviation of the input of M1, in 1e-3 rad")
guide_number("n_lags", n_lags, "%d", "lags that the default call of predict_spinal_levels() needs: 60 + 4 + 500")
guide_number("vbar_0_pct", 100 * cg$vbar, "%.2f", "share vbar of an uncorrelated series centered over 320 values, in percent")
guide_number("vbar_p_pct", 100 * vbar_p, "%.1f", "share vbar of a fractionally integrated series with d = 0.3 centered over 320 values, in percent")
guide_number("gam_ratio_1", cg$gam[2] / sd_u^2, "%.4f", "centered autocovariance of the uncorrelated input beyond lag 0, as a ratio to sd^2")
guide_number("c_hat_0", c_hat[1] / sd_u^2, "%.4f", "sample autocovariance at lag 0 of the generated input, as a ratio to sd^2")
guide_number("c_hat_max", max(abs(c_hat[-1])) / sd_u^2, "%.2f", "largest absolute sample autocovariance at lags 1 to 4 of the generated input, as a ratio to sd^2")
t_m1 <- Sys.time()

## ---- prediction-m1 ----
lin <- linearize(m1)
pred <- predict_spinal_levels(lin, cg$gam, n_steps = n_keep)
round(pred$levels, 4)
round(pred$weights, 3)
spinal_lambda_l_m1(pred)

## ---- hidden: numbers-prediction-m1 ----
guide_number("pred_m1_seconds", as.numeric(difftime(Sys.time(), t_m1, units = "secs")), "%.0f",
             "run time of the block that linearizes M1 and predicts its levels, in seconds")
if (!identical(lin$model, "M1") || length(lin$b_vec) != 2L) stop("the linearization of M1 was expected to have a state of two variables")
if (!identical(names(pred), c("levels", "weights", "period", "initial_rank"))) stop("the result was expected to have four elements")
if (abs(sum(pred$weights) - 1) > 1e-12) stop("the weights were expected to sum to 1")
if (any(pred$weights[c("D=400", "D=500")] != 0)) stop("the nodes above the record were expected to have a weight of zero")
lambda_m1 <- spinal_lambda_l_m1(pred)
if (!isTRUE(all.equal(lambda_m1, unname(pred$levels[["k=4"]] - pred$levels[["k=3"]]) / lin$period))) {
  stop("spinal_lambda_l_m1() was expected to equal (y(4) - y(3)) / T*")
}
# separations that each node receives, with the rule of predict_spinal_levels()
grid <- spinal_d_grid()
if (!identical(grid, c(2L, 4L, 6L, 8L, 10L, 14L, 20L, 30L, 50L, 80L, 120L, 200L, 300L, 400L, 500L))) {
  stop("the default nodes differ from those quoted in the chapter")
}
d_all <- seq(2L, n_keep - 2L, by = 2L)
near <- vapply(d_all, function(d) which.min(abs(grid - d)), integer(1))
i_max <- which.max(pred$weights)
ell_idx <- seq(1L, length(lin$ell) - 1L, by = 5L)
guide_number("n_ell", length(ell_idx), "%d", "local times of the default subsample, ell_step = 5")
guide_number("n_grid", length(lin$ell), "%d", "local times of the linearization, n_grid of linearize()")
guide_number("y2", pred$levels[["k=2"]], "%.3f", "level of M1 at 2 steps, uncorrelated input")
guide_number("y3", pred$levels[["k=3"]], "%.3f", "level of M1 at 3 steps, uncorrelated input")
guide_number("y4", pred$levels[["k=4"]], "%.3f", "level of M1 at 4 steps, uncorrelated input")
guide_number("period_m1", pred$period, "%.4f", "step duration T* of M1 on the limit cycle, in model time units")
guide_number("rank_m1", pred$initial_rank, "%d", "dimension on which the epochs of M1 are matched")
guide_number("lambda_m1", lambda_m1, "%.4f", "predicted long-term exponent of M1 for an uncorrelated input, per unit of model time")
guide_number("n_separations", length(d_all), "%d", "even separations from 2 to 298 steps")
guide_number("w_max", max(pred$weights), "%.3f", "largest weight of a node, example of M1")
guide_number("w_max_node", grid[i_max], "%d", "node with the largest weight, in steps")
guide_number("w_max_from", min(d_all[near == i_max]), "%d", "smallest separation assigned to the node with the largest weight")
guide_number("w_max_to", max(d_all[near == i_max]), "%d", "largest separation assigned to the node with the largest weight")
# check of a statement of the text on the function: an autocovariance multiplied by s^2
# adds ln(s) to every level and leaves the weights and the differences of levels unchanged
pred_scale <- predict_spinal_levels(lin, 4 * cg$gam, n_steps = n_keep)
if (!isTRUE(all.equal(unname(pred_scale$levels - pred$levels), rep(log(2), 3), tolerance = 1e-8)) ||
    !isTRUE(all.equal(pred_scale$weights, pred$weights)) ||
    abs(spinal_lambda_l_m1(pred_scale) - lambda_m1) > 1e-8) {
  stop("an input twice as large was expected to add ln 2 to each level and to keep the weights")
}

## ---- hidden: figure-weights ----
guide_figure("weights", width_cm = 15.5, height_cm = 4.2, draw = function() {
  x <- seq_along(grid)
  par(mar = c(3.4, 4.6, 0.6, 0.6), mgp = c(2.3, 0.6, 0), tcl = -0.3, las = 1)
  plot(x, pred$weights, type = "n", axes = FALSE, xlab = "Node of the separation (steps)",
       ylab = "", ylim = c(0, 0.25), xlim = c(0.6, length(grid) + 0.4))
  title(ylab = "Weight", line = 3.2)
  abline(h = 0, col = col[["gray"]], lwd = 0.5)
  segments(x, 0, x, pred$weights, col = col[["blue"]], lwd = 1.6)
  points(x, pred$weights, pch = 16, col = col[["blue"]], cex = 0.9)
  axis(1, at = x, labels = grid, col = col[["gray"]], col.axis = "black")
  axis(2, at = seq(0, 0.25, by = 0.05), col = col[["gray"]], col.axis = "black")
})

## ---- hidden: timer-m2 ----
t_m2 <- Sys.time()

## ---- model-m2 ----
m2 <- model_m2()
lin2 <- linearize(m2)
st <- spinal_settings_m2(lin2, n_steps = 600)
c(n_past = st$n_past, n_nodes = length(st$d_grid))
spinal_n_past(lin2, rho = max(Mod(lin2$eig)))
n_total2 <- transient_cycles(m2) + 600L
n_lags2 <- st$n_past + max(st$k_levels) + max(st$d_grid)
rho_0 <- fi_autocorrelation(d = 0, n_lags = n_lags2)
cg2 <- centered_autocovariance(rho_0, n_center = n_total2, sd = 5e-5)
pred2 <- predict_spinal_levels(lin2, cg2$gam, n_steps = 600,
                               k_levels = st$k_levels, d_grid = st$d_grid,
                               n_past = st$n_past, maps = st$maps,
                               separation_step = st$separation_step,
                               initial_rank_tol = st$initial_rank_tol)
c(n_lags = n_lags2, initial_rank = pred2$initial_rank)
round(pred2$levels[c("k=8", "k=14", "k=20")], 4)
spinal_lambda_l_lsq(pred2)

## ---- hidden: numbers-model-m2 ----
guide_number("pred_m2_seconds", as.numeric(difftime(Sys.time(), t_m2, units = "secs")), "%.0f",
             "run time of the block that linearizes M2, reads its settings and predicts its levels, in seconds")
mu <- max(Mod(lin2$eig))
if (!identical(names(st), c("maps", "separation_step", "k_levels", "n_past", "initial_rank_tol",
                            "d_grid", "d_grid_sensitivity"))) stop("the settings were expected to have seven elements")
if (!identical(st$maps, list(diag(4), diag(4)))) stop("the maps of M2 were expected to be two identity matrices of size 4")
if (!identical(st$separation_step, 1L) || !identical(st$k_levels, 8:20) || !identical(st$initial_rank_tol, 1e-8)) {
  stop("the settings of M2 differ from those of the table of the chapter")
}
if (!identical(st$d_grid, c(2:20, 30L, 50L, 80L, 120L, 200L, 300L, 400L, 500L))) stop("the grid of M2 differs from that of the table of the chapter")
if (!identical(st$d_grid, st$d_grid_sensitivity)) stop("the two grids were expected to be equal for a record of 600 steps")
st1200 <- spinal_settings_m2(lin2, 1200L)
if (!identical(st1200$d_grid, c(st$d_grid, 700L, 900L, 1000L)) || !identical(st1200$d_grid_sensitivity, st$d_grid)) {
  stop("the grids of M2 for 1,200 steps differ from those of the table of the chapter")
}
n_first <- 1L
while (0.923^n_first > 1e-4) n_first <- n_first + 1L
norm_ratio <- function(n) {
  v <- lin2$b_vec
  for (i in seq_len(n)) v <- as.numeric(lin2$j_map %*% v)
  sqrt(sum(v^2)) / sqrt(sum(lin2$b_vec^2))
}
n_second <- 1L
while (norm_ratio(n_second) > 1e-4) n_second <- n_second + 1L
if (spinal_n_past(lin2) != max(n_first, n_second)) stop("n_past was expected to be the larger of the two conditions")
guide_number("m2_state", length(lin2$b_vec), "%d", "state variables after contact of M2")
guide_number("m2_n_past", st$n_past, "%d", "past inputs of spinal_settings_m2() for model_m2() with its default arguments")
guide_number("m2_n_first", n_first, "%d", "smallest n with 0.923^n <= 1e-4")
guide_number("m2_n_second", n_second, "%d", "smallest n with |J^n b| <= 1e-4 |b|, model_m2() with its default arguments")
guide_number("m2_mu", mu, "%.3f", "largest eigenvalue modulus of the cycle map of model_m2() with its default arguments")
guide_number("m2_n_own", spinal_n_past(lin2, rho = mu), "%d", "past inputs with rho set to the largest eigenvalue modulus of the configuration")
guide_number("m2_n_nodes", length(st$d_grid), "%d", "nodes of the grid of M2 for a record of 600 steps")
k2 <- as.numeric(sub("^k=", "", names(pred2$levels)))
if (!isTRUE(all.equal(spinal_lambda_l_lsq(pred2), 2 * unname(coef(lm(pred2$levels ~ k2))[2])))) {
  stop("spinal_lambda_l_lsq() was expected to equal twice the least-squares slope of the levels")
}
guide_number("m2_n_transient", transient_cycles(m2), "%d", "transient cycles of model_m2() with its default arguments")
guide_number("m2_n_total", n_total2, "%d", "values of the input of the example of M2, transient cycles included")
# a record of 1,200 values serves the node at 1,000 steps and would not serve one at 1,100
if (st$n_past + max(st$k_levels) + 1000L > 1200L || st$n_past + max(st$k_levels) + 1100L <= 1200L) {
  stop("the node at 1,000 steps was expected to be the last one that 1,200 lags serve")
}
guide_number("m2_n_lags", n_lags2, "%d", "lags that the prediction of M2 needs")
guide_number("m2_sd", 1e5 * 5e-5, "%.0f", "standard deviation of the input of M2, in 1e-5 leg length")
guide_number("m2_rank", pred2$initial_rank, "%d", "dimension on which the epochs of M2 are matched")
guide_number("m2_n_levels", length(pred2$levels), "%d", "levels of the prediction of M2")
guide_number("m2_y8", pred2$levels[["k=8"]], "%.3f", "level of M2 at 8 steps, uncorrelated input")
guide_number("m2_y20", pred2$levels[["k=20"]], "%.3f", "level of M2 at 20 steps, uncorrelated input")
guide_number("m2_lambda", spinal_lambda_l_lsq(pred2), "%.4f", "predicted long-term exponent of model_m2() with its default arguments for an uncorrelated input, per stride")
# without initial_rank_tol, the matching of M2 stops in solve(); small call
msg_singular <- msg_of(suppressWarnings(
  predict_spinal_levels(lin2, cg2$gam, n_steps = 600, k_levels = 8:9, d_grid = c(2L, 50L),
                        n_past = st$n_past, maps = st$maps, separation_step = 1L)))
if (!grepl("^system is computationally singular", msg_singular)) stop("M2 without initial_rank_tol was expected to stop in solve()")

## ---- matching ----
k_window <- 4:8
rho_ar8 <- ar1_autocorrelation(phi = 0.5, n_lags = max(k_window))
window_slope_half_log_v(rho_ar8, k_window)
d_phi <- match_fi_to_ar1(phi = 0.5, k_window = k_window)
d_phi
window_slope_half_log_v(fi_autocorrelation(d_phi, max(k_window)), k_window)

## ---- hidden: numbers-matching ----
slope_ar <- window_slope_half_log_v(rho_ar8, k_window)
slope_fi <- window_slope_half_log_v(fi_autocorrelation(d_phi, max(k_window)), k_window)
if (abs(slope_ar - slope_fi) > 1e-8) stop("the matched series was expected to have the slope of the AR(1) series")
v_of <- function(rho, s) s * rho[1] + 2 * sum((s - seq_len(s - 1L)) * rho[seq_len(s - 1L) + 1L])
y_ar <- vapply(k_window, function(s) 0.5 * log(v_of(rho_ar8, s)), numeric(1))
if (!isTRUE(all.equal(slope_ar, unname(coef(lm(y_ar ~ k_window))[2])))) stop("the slope was expected to be the least-squares slope of 0.5 ln v(K)")
guide_number("slope_ar", slope_ar, "%.4f", "slope of 0.5 ln v(K) over K = 4 to 8 for the AR(1) series, phi = 0.5")
guide_number("d_phi", d_phi, "%.3f", "d of the fractionally integrated series with the slope of the AR(1) series, phi = 0.5, K = 4 to 8")

## ---- errors ----
show_error <- function(expr) {
  writeLines(strwrap(tryCatch(expr, error = conditionMessage), 78, exdent = 2))
}
show_error(predict_spinal_levels(lin, cg$gam[1:300], n_steps = n_keep))
show_error(predict_spinal_levels(lin2, cg2$gam, n_steps = 600))

## ---- hidden: checks-errors ----
# the other messages of the table of errors
expect_message <- function(got, expected) {
  if (!identical(got, expected)) stop("unexpected error message: ", got)
}
expect_message(msg_of(fi_autocorrelation(d = 0.5, n_lags = 10)), "d must satisfy -1 < d < 1/2")
expect_message(msg_of(fi_autocorrelation(d = -1, n_lags = 10)), "d must satisfy -1 < d < 1/2")
expect_message(msg_of(centered_autocovariance(rho_0[1:100], n_center = n_total, sd = sd_u)),
               "rho needs at least n_center lags")
expect_message(msg_of(match_fi_to_ar1(phi = 0.99, k_window = k_window)),
               "f() values at end points not of opposite sign")
expect_message(msg_of(ar1_autocorrelation(phi = 1, n_lags = 10)), "|phi| must be below 1")
expect_message(msg_of(sample_autocovariance(u$values, n_lags = n_total + 1)),
               "n_lags exceeds the length of the series")
expect_message(msg_of(spinal_settings_m2(lin, n_steps = 600)),
               "the settings of update 1 are those of M2")
expect_message(msg_of(spinal_lambda_l_m1(list(levels = c("k=2" = 0, "k=3" = 0), period = 1))),
               "subscript out of bounds")
expect_message(msg_of(window_slope_half_log_v(fi_autocorrelation(0, 5), 4:8)),
               "NA/NaN/Inf in 'y'")
expect_message(msg_of(spinal_n_past(lin2, rho = 0.5, tol = 1e-12, n_max = 20L)),
               "the norm condition does not hold up to n_max")
