# Title: Worked example of the chapter "Linearization at the fixed point" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: linearizes model M1 of walkbench 0.6.2 at its fixed point with linearize(),
#   prints the matrices of the linear model of the cycle map, compares the response of the step
#   duration that the linear model predicts with the response simulated for one small input
#   value, for a constant input and for one large input value, linearizes M1 at its other
#   injection points and model M2 with its default arguments, and produces the error of an
#   injection point without linearization. The example is a demonstration computed for the
#   guide. It draws no random number, so it uses none of the seeds reserved for the chapter
#   (cells 80 to 89 of the block 90,000,000 to 92,099,999 of the guide). The blocks whose label
#   starts with "hidden:" write the signature of the function, register the numbers quoted in
#   the chapter, check the statements of the text and draw its figure; the other blocks are
#   printed in the chapter with their output.
# Dependencies: walkbench 0.6.2 (loaded by the driver), stats, graphics, grDevices
# Input: none
# Output: generated/08_linearization/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_response.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/08_linearization.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
guide_signature("linearize")

## ---- linearize-m1 ----
m1 <- model_m1()
lin <- linearize(m1)
round(lin$j_map, 4)
round(lin$b_vec, 4)
round(lin$a_vec, 2)
c(e = lin$e_sc, static_gain = lin$static_gain)
Mod(lin$eig)

## ---- hidden: numbers-linearize-m1 ----
fields <- c("x_star", "period", "j_map", "b_vec", "a_vec", "e_sc", "static_gain", "eig",
            "impulse_T", "ell", "y_star", "f_star", "phi", "psi", "m1", "m2", "coord_maps",
            "h", "injection", "model", "produced_by")
if (!identical(names(lin), fields)) stop("the fields of the linearization differ from the table of the chapter")
fp <- m1$fixed_point()
if (!identical(lin$x_star, fp$x_star) || !identical(lin$period, fp$period)) {
  stop("x_star and period were expected to be those of fixed_point()")
}
# Eq. of the static gain and of the response of the step duration, from the fields
nx <- length(lin$x_star)
gain_check <- lin$e_sc + sum(lin$a_vec * solve(diag(nx) - lin$j_map, lin$b_vec))
g_check <- c(lin$e_sc, vapply(1:10, function(j) {
  p <- diag(nx)
  for (i in seq_len(j - 1L)) p <- p %*% lin$j_map
  sum(lin$a_vec * (p %*% lin$b_vec))
}, numeric(1)))
if (abs(gain_check - lin$static_gain) > 1e-12 || max(abs(g_check - lin$impulse_T)) > 1e-12) {
  stop("static_gain or impulse_T differs from the equations of the chapter")
}
if (!identical(dim(lin$y_star), c(131L, 4L)) || !identical(dim(lin$phi), c(131L, 4L, 2L)) ||
    !identical(dim(lin$psi), c(131L, 4L)) || length(lin$ell) != 131L ||
    lin$ell[1] != 0 || lin$ell[131] != lin$period) {
  stop("the dimensions of the fields differ from the table of the chapter")
}
if (!identical(lin$m1, m1$coord_maps[[1]]) || !identical(lin$m2, m1$coord_maps[[2]])) {
  stop("m1 and m2 were expected to be the coordinate maps of the model")
}
if (max(abs(lin$f_star[1, ] - m1$rhs(lin$y_star[1, ]))) > 0) stop("f_star was expected to be rhs(y_star)")
guide_number("n_fields", length(fields), "%d", "fields of the list returned by linearize()")
guide_number("nx_m1", nx, "%d", "size of the reduced state of M1")
guide_number("e", lin$e_sc, "%.3f", "coefficient e of M1 at the default injection point, in time units per rad")
guide_number("static_gain", lin$static_gain, "%.3f", "static gain of M1 at the default injection point, in time units per rad")
guide_number("eig_mod", Mod(lin$eig[1]), "%.3f", "modulus of the eigenvalues of J of M1")
# a power of ten as LaTeX code, for the input values quoted in the text
power_of_ten <- function(x) {
  p <- round(log10(x))
  if (abs(x / 10^p - 1) > 1e-12) stop("a power of ten was expected")
  sprintf("10^{%d}", p)
}

## ---- response ----
round(lin$impulse_T, 3)
duration_after <- function(u3, n = 14) {
  u <- simaci_input(replace(numeric(n), 3, u3), source = "single value")
  simulate_run(m1, u, n_cycles = n)$events$duration
}
h <- 1e-5
simulated <- (duration_after(h) - duration_after(-h))[3:13] / (2 * h)
relative <- simulated / lin$impulse_T - 1
signif(max(abs(relative[1:10])), 2)
signif(relative[11], 2)

## ---- hidden: numbers-response ----
if (max(abs(relative[1:5])) > 0.01) stop("the linear response was expected within 1% of the simulation at lags 0 to 4")
if (which.max(abs(lin$impulse_T)) != 2L) stop("the largest response was expected at lag 1")
guide_number("g1", lin$impulse_T[2], "%.2f", "response of the step duration at lag 1, in time units per rad")
guide_number("rel_max_0_9", 1e6 * max(abs(relative[1:10])), "%.1f", "largest relative difference between the simulated and the linear response at lags 0 to 9, in 1e-6")
guide_number("rel_10", 1e4 * abs(relative[11]), "%.1f", "relative difference between the simulated and the linear response at lag 10, in 1e-4")
guide_number("abs_10", 1e5 * abs(simulated[11] - lin$impulse_T[11]), "%.1f", "absolute difference between the simulated and the linear response at lag 10, in 1e-5 time units per rad")
guide_number("pulse_h", power_of_ten(h), "%s", "input value of the small-input comparison, in rad, as LaTeX code")
guide_number("noise_gain", 1e5 * 1e-9 / (2 * h), "%.0f", "change of the simulated response for an error of 1e-9 time units in the difference of two step durations, in 1e-5 time units per rad")

## ---- static-gain ----
c(static_gain = lin$static_gain, sum_lags_0_to_10 = sum(lin$impulse_T))
u_const <- simaci_input(rep(1e-4, 60), source = "constant")
run_const <- simulate_run(m1, u_const, n_cycles = 60)
(run_const$events$duration[60] - lin$period) / 1e-4

## ---- hidden: numbers-static-gain ----
gain_sim <- (run_const$events$duration[60] - lin$period) / 1e-4
if (abs(gain_sim / lin$static_gain - 1) > 0.005) stop("the simulated static response was expected within 0.5% of the static gain")
guide_number("gain_sum", sum(lin$impulse_T), "%.3f", "sum of impulse_T over lags 0 to 10, in time units per rad")
guide_number("gain_sim", gain_sim, "%.3f", "change of the step duration per unit of a constant input of 1e-4 rad after 60 cycles, in time units per rad")
guide_number("gain_rel", 100 * abs(gain_sim / lin$static_gain - 1), "%.1f", "relative difference between the simulated static response and the static gain, in percent")
guide_number("const_u", power_of_ten(u_const$values[1]), "%s", "constant input of the static gain example, in rad, as LaTeX code")
guide_number("const_n", 60, "%d", "cycles of the run with a constant input")

## ---- large-input ----
ratio <- (duration_after(0.01)[3:13] - lin$period) / (0.01 * lin$impulse_T)
round(ratio[1:4], 2)

## ---- hidden: numbers-large-input ----
if (abs(ratio[3] - 1) < 0.2) stop("a departure from the linear response was expected at lag 2 for an input of 0.01 rad")
guide_number("large_u", 0.01, "%.2f", "large input value, in rad")
guide_number("large_ratio0", ratio[1], "%.2f", "simulated over linear response at lag 0 for an input of 0.01 rad")
guide_number("large_ratio1", ratio[2], "%.2f", "simulated over linear response at lag 1 for an input of 0.01 rad")
guide_number("large_ratio2", ratio[3], "%.2f", "simulated over linear response at lag 2 for an input of 0.01 rad")

## ---- injection-points ----
points <- c("foot_strike_timing", "foot_strike_full", "slope")
sapply(points, function(p) max(abs(linearize(m1, injection = p)$psi)) > 0)
tryCatch(linearize(m1, injection = "terrain"), error = conditionMessage)

## ---- linearize-m2 ----
lin2 <- linearize(model_m2())
shown <- c("x_star", "j_map", "b_vec", "a_vec", "eig", "impulse_T",
           "y_star", "f_star", "phi", "psi")
size <- function(f) paste(dim(as.array(f)), collapse = " x ")
noquote(sapply(lin2[shown], size))

## ---- hidden: numbers-linearize-m2 ----
if (!identical(names(lin2), fields)) stop("the fields of the linearization of M2 differ from those of M1")
if (lin2$model != "M2" || lin2$injection != "floor_height" ||
    length(model_m2()$injection_points) != 1L) {
  stop("M2 was expected with the single injection point floor_height")
}
if (length(lin2$x_star) != 4L || max(abs(lin2$psi)) != 0) stop("M2 was expected with a reduced state of size 4 and psi equal to zero")
guide_number("nx_m2", length(lin2$x_star), "%d", "size of the reduced state of M2")

## ---- hidden: errors ----
# the messages of the table of errors of the chapter, each produced by a call
message_of <- function(expr) {
  tryCatch({ expr; "no error" }, error = conditionMessage)
}
for (p in c("terrain", "foot_strike_geometric", "floor_height")) {
  if (message_of(linearize(m1, injection = p)) !=
      paste("injection point without linearization:", p)) {
    stop("the message of linearize() differs from the table of the chapter")
  }
}
if (m1$injection_points$terrain$linearizable || !is.null(m1$injection_points$floor_height)) {
  stop("terrain was expected without linearization, and floor_height absent from M1")
}

## ---- hidden: numbers-notes ----
# sensitivity of the result to the step of the finite differences
alt <- lapply(c(1e-5, 1e-7), function(hh) linearize(m1, h = hh))
guide_number("h_dj", 1e6 * max(vapply(alt, function(l) max(abs(l$j_map - lin$j_map)), numeric(1))), "%.1f", "largest change of an entry of J when h is 1e-5 or 1e-7 in place of 1e-6, in 1e-6")
guide_number("h_dg", 1e6 * max(vapply(alt, function(l) abs(l$static_gain - lin$static_gain), numeric(1))), "%.1f", "largest change of the static gain when h is 1e-5 or 1e-7 in place of 1e-6, in 1e-6 time units per rad")
# eigenvalues of linearize() and of fixed_point()
guide_number("eig_diff", 1e6 * abs(Mod(lin$eig[1]) - Mod(fp$eig[1])), "%.1f", "difference between the eigenvalue moduli of linearize() and of fixed_point(), in 1e-6")
if (transient_cycles(m1) != transient_from_radius(max(Mod(lin$eig)))) stop("the two sets of eigenvalues were expected to give the same number of transient cycles")
guide_number("n_transient", transient_cycles(m1), "%d", "transient cycles of M1")
# n_grid changes the grid of local times only
lin_coarse <- linearize(m1, n_grid = 11L)
if (!identical(dim(lin_coarse$phi), c(11L, 4L, 2L)) || !identical(lin_coarse$j_map, lin$j_map)) {
  stop("n_grid was expected to change the grid of local times only")
}

## ---- hidden: figure-response ----
col <- guide_colors()
guide_figure("response", width_cm = 15.5, height_cm = 5.9, draw = function() {
  lags <- 0:10
  sim_large <- (duration_after(0.01)[3:13] - lin$period) / 0.01
  panel <- function(sim, tag, ylab, legend_on) {
    par(mar = c(0.9, if (nzchar(ylab)) 5.4 else 3.2, 1.8, 0.5))
    plot(NA, xlim = c(-0.4, 10.4), ylim = c(-15, 15), axes = FALSE, xlab = "", ylab = "")
    abline(h = 0, col = col[["gray"]], lwd = 0.5)
    segments(lags, 0, lags, lin$impulse_T, col = col[["blue"]], lwd = 1.2)
    points(lags, lin$impulse_T, pch = 1, col = col[["blue"]], cex = 1.5, lwd = 1.3)
    points(lags, sim, pch = 16, col = col[["vermilion"]], cex = 0.8)
    axis(2, at = seq(-15, 15, by = 5), col = col[["gray"]])
    axis(1, at = lags, labels = FALSE, col = col[["gray"]])
    axis(1, at = seq(0, 10, by = 2), col = col[["gray"]])
    if (nzchar(ylab)) mtext(ylab, side = 2, line = 2.9, las = 0)
    title(main = tag, adj = 0, font.main = 1, cex.main = 1, line = 0.5)
    if (legend_on) {
      legend("topright", legend = c("linear model (impulse_T)", "simulation"),
             pch = c(1, 16), col = c(col[["blue"]], col[["vermilion"]]),
             pt.cex = c(1.5, 0.8), pt.lwd = 1.3, bty = "n", cex = 0.95)
    }
  }
  layout(matrix(1:2, 1, 2), widths = c(8.17, 7.33))
  par(oma = c(3.0, 0, 0.2, 0), mgp = c(2.4, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  panel(simulated, "(a) Input values of 1e-5 and -1e-5 rad",
        "Change of the step duration\nper unit of input (time units per rad)", TRUE)
  panel(sim_large, "(b) Input value of 0.01 rad", "", FALSE)
  mtext("Lag (cycles after the input value)", side = 1, outer = TRUE, line = 1.8)
})
