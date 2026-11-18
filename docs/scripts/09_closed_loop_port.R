# Title: Worked example of the chapter "Closed-loop port" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the writer agent of Chapter 9 (Claude Fable 5.2), a writer agent of
#   the guide session of SIM-ACI, dispatched by the guide session (Claude Fable 5.2), without a
#   code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-13 (written in the night of 2026-11-12 to 2026-11-13, machine clock)
# Description: applies the closed-loop port of walkbench 0.6.2 to model M1. The script
#   linearizes the model, shows what a controller receives and returns, compares a run under
#   the controller of variant H3a at a gain of zero with the open-loop run, runs the
#   controllers of variants H3a and H3b at a gain of 0.1, computes the gain scales, the
#   matrix, the spectral radius and the largest stable gain of the linearized loop, gives the
#   number of transient cycles under a controller, runs one closed-loop trial through the run
#   driver and runs a controller defined by the user. The example is a demonstration computed
#   for the guide. Every exogenous series is uncorrelated (input_arfima() with d = 0) and has
#   one standard deviation, 0.002 rad. The seeds lie in the block of the guide, 90,000,000 to
#   92,099,999, in two of the cells 90 to 99 reserved for this chapter: the series built
#   directly use 90,009,001 and 90,009,002 (cell 90), and the trial uses the seed rule with
#   the master seed 90,000,000, cell 91 and replicate 1. The blocks whose label starts with "hidden:"
#   write the signatures of the functions, register the numbers quoted in the chapter, check
#   the statements of the text and draw its two figures; the other blocks are printed in the
#   chapter with their output.
# Dependencies: walkbench 0.6.2 (loaded by the driver), stats, graphics, grDevices
# Input: none
# Output: generated/09_closed_loop_port/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_radius.pdf, fig_asynchrony.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/09_closed_loop_port.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
for (f in c("make_metronome", "make_controller_h3a", "make_controller_h3b", "gain_scale",
            "closed_loop_matrix", "closed_loop_radius", "stable_gain_limit",
            "transient_cycles_loop")) guide_signature(f)

## ---- model ----
m1 <- model_m1()
lin <- linearize(m1)
round(c(T_star = lin$period, b_1 = lin$b_vec[1], e = lin$e_sc,
        G = lin$static_gain), 4)
n_transient <- transient_cycles(m1)
n_keep <- 300L
n_total <- n_transient + n_keep
w <- input_arfima(n_total, d = 0, sd = 0.002, seed = 90009001,
                  n_transient = n_transient)

## ---- hidden: numbers-model ----
guide_number("n_transient", n_transient, "%d", "transient cycles of M1 in open loop, transient_cycles(model_m1())")
guide_number("n_keep", n_keep, "%d", "kept cycles of the example")
guide_number("n_total", n_total, "%d", "cycles simulated in the runs of the example")
guide_number("w_sd", 1000 * w$params$sd, "%.0f", "sd of the exogenous series on the kept cycles, in 1e-3 rad")
guide_number("radius_open", max(Mod(eigen(lin$j_map, only.values = TRUE)$values)), "%.3f",
             "spectral radius of the matrix J of M1, closed_loop_radius() at a gain of zero in H3a")
if (lin$injection != m1$default_injection) stop("the linearization was expected at the default injection point")
if (m1$reduced_state_names[1] != "theta" || model_m2()$reduced_state_names[1] != "theta") {
  stop("the first coordinate of the reduced state was expected to be theta in both models")
}
if (m1$corrected_index != 1L || model_m2()$corrected_index != 1L) stop("the member corrected_index was expected to be 1 in both models")

## ---- contract ----
seen <- NULL
spy <- function(k, info) {
  if (k == 2) seen <<- info        # keeps what the controller receives
  list(u = 0)                      # the smallest valid return value
}
run_spy <- simulate_run(m1, n_cycles = 3, controller = spy)
str(lapply(seen, function(v) round(as.numeric(v), 4)), digits.d = 5)

## ---- hidden: checks-contract ----
if (!all(is.na(run_spy$events[, c("u_exo", "u_corr", "deviation")]))) stop("the optional columns were expected to be empty")
first <- NULL
run_first <- simulate_run(m1, n_cycles = 1, controller = function(k, info) {
  first <<- info
  list(u = 0)
}, metronome = make_metronome(lin$period))
if (!is.null(first$y_pre) || !is.na(first$duration_prev) || !identical(first$asynchrony, 0) ||
    first$t_contact != 0) {
  stop("the list info of the first call differs from the description of the text")
}
if (!isTRUE(all.equal(unname(first$y_post), unname(m1$fixed_point()$y0), tolerance = 0)) ||
    !isTRUE(all.equal(unname(first$x_post), unname(m1$fixed_point()$y0[1:2]), tolerance = 0))) {
  stop("the first post-contact states were expected to be those of the initial state of the run")
}
if (length(seen$x_post) != 2L || length(seen$y_post) != 4L || length(seen$y_pre) != 4L ||
    !is.na(seen$asynchrony)) stop("the list info of M1 differs from the description of the text")
if (!identical(unname(seen$x_post), unname(seen$y_post[1:2]))) stop("x_post was expected to hold the first two values of y_post")
if (!identical(seen$t_contact, run_spy$events$t_end[1]) ||
    !identical(seen$duration_prev, run_spy$events$duration[1])) {
  stop("t_contact and duration_prev were expected to describe contact 1 and cycle 1")
}
if (max(abs(run_spy$events$duration - lin$period)) > 1e-6) stop("an input of zero was expected to keep the model on its limit cycle")
# list info of M2: the reduced state and the full state are the same four values
seen_m2 <- NULL
run_m2 <- simulate_run(model_m2(), n_cycles = 2, controller = function(k, info) {
  if (k == 2) seen_m2 <<- info
  list(u = 0)
})
if (length(seen_m2$x_post) != 4L || !identical(seen_m2$x_post, seen_m2$y_post) ||
    !identical(names(seen_m2), names(seen))) stop("the list info of M2 differs from the description of the text")

## ---- h3a ----
run_open <- simulate_run(m1, w, n_cycles = n_total)
ctrl_zero <- make_controller_h3a(w, g = 0, lin)
run_zero <- simulate_run(m1, n_cycles = n_total, controller = ctrl_zero)
identical(run_zero$samples, run_open$samples)
ctrl_a <- make_controller_h3a(w, g = 0.1, lin)        # scale "deadbeat"
run_a <- simulate_run(m1, n_cycles = n_total, controller = ctrl_a)
ev_a <- run_a$events
round(1000 * ev_a[1:4, c("u", "u_exo", "u_corr", "deviation")], 4)

## ---- hidden: numbers-h3a ----
if (!identical(run_zero$events$u, run_open$events$u) ||
    !identical(run_zero$events$duration, run_open$events$duration) ||
    !identical(run_zero$samples, run_open$samples)) {
  stop("the run of H3a at a gain of zero was expected to equal the open-loop run")
}
if (run_open$fell || run_open$n_cycles_done != n_total) stop("the open-loop run was expected to complete")
if (!is.null(run_zero$input) || is.null(run_open$input)) stop("the field input was expected to be empty in the closed-loop run only")
if (!all(is.na(run_open$events$u_exo)) || !all(is.na(run_open$events$deviation)) ||
    !all(is.na(run_open$events$asynchrony)) || !all(is.na(run_open$events$beat))) {
  stop("the columns of the closed-loop port were expected to be empty in open loop")
}
guide_number("gain_a", 0.1, "%.1f", "gain g of the run of H3a")
if (run_a$fell || run_a$n_cycles_done != n_total) stop("the run of H3a was expected to complete")
if (!identical(ev_a$u, ev_a$u_exo + ev_a$u_corr) || !identical(ev_a$u_exo, w$values) ||
    !identical(ev_a$deviation, ev_a$theta_post - lin$x_star[1])) {
  stop("the columns of the run of H3a differ from the description of the text")
}
dx_a <- cbind(ev_a$theta_post - lin$x_star[1], ev_a$theta_dot_post - lin$x_star[2])
corr_a <- -0.1 * as.numeric(dx_a %*% lin$j_map[1, ]) / lin$b_vec[1]
if (!isTRUE(all.equal(ev_a$u_corr, corr_a, tolerance = 1e-12))) stop("u_corr of H3a differs from the formula of the deadbeat scale")
guide_number("a_sd_exo", 1000 * sd(ev_a$u_exo), "%.2f", "sd of u_exo over the run of H3a, in 1e-3 rad")
guide_number("a_sd_corr", 1000 * sd(ev_a$u_corr), "%.2f", "sd of u_corr over the run of H3a, in 1e-3 rad")
# shift scale: the correction is -g delta / (c'b)
run_s <- simulate_run(m1, n_cycles = 40, controller = make_controller_h3a(w, g = 0.1, lin, scale = "shift"))
if (!isTRUE(all.equal(run_s$events$u_corr, -0.1 * run_s$events$deviation / lin$b_vec[1], tolerance = 1e-12))) {
  stop("u_corr of H3a differs from the formula of the shift scale")
}
# an input given beside a controller is checked and stored, and the model does not apply it
other <- input_arfima(n_total, d = 0, sd = 0.002, seed = 90009002, n_transient = n_transient)
run_both <- simulate_run(m1, input = other, n_cycles = 60, controller = ctrl_a)
if (!identical(run_both$events$u, ev_a$u[1:60]) || !identical(run_both$input, other)) {
  stop("an input given beside a controller was expected to be stored and not applied")
}

## ---- h3b ----
met <- make_metronome(lin$period)
ctrl_b <- make_controller_h3b(w, kappa = 0.1, lin)    # scale "static"
run_b <- simulate_run(m1, n_cycles = n_total, controller = ctrl_b,
                      metronome = met)
ev_b <- run_b$events
round(ev_b[2:4, c("cycle", "t_end", "beat", "asynchrony", "deviation")], 4)
run_free <- simulate_run(m1, w, n_cycles = n_total, metronome = met)
round(run_free$events$asynchrony[c(1, 2, n_total)], 4)

## ---- hidden: numbers-h3b ----
guide_number("gain_b", 0.1, "%.1f", "gain kappa of the run of H3b")
if (!inherits(met, "simaci_metronome") || !identical(names(met), c("period", "beat")) ||
    !identical(met$beat(0:2), (0:2) * lin$period)) stop("the metronome object differs from the description of the text")
if (run_b$fell || run_b$n_cycles_done != n_total) stop("the run of H3b was expected to complete")
if (!identical(run_b$metronome_period, lin$period) || !is.na(run_open$metronome_period)) {
  stop("the field metronome_period differs from the description of the text")
}
if (!identical(ev_b$asynchrony, ev_b$t_end - ev_b$beat) ||
    !identical(ev_b$deviation, c(0, ev_b$asynchrony[-n_total]))) {
  stop("the columns asynchrony and deviation of the run of H3b differ from the description of the text")
}
if (!isTRUE(all.equal(ev_b$u_corr, -0.1 * ev_b$deviation / lin$static_gain, tolerance = 1e-12))) {
  stop("u_corr of H3b differs from the formula of the static scale")
}
if (!identical(ev_b$u_exo, w$values)) stop("u_exo of H3b was expected to equal the series")
if (!all(is.na(run_free$events$deviation))) stop("the open-loop run was expected to have no deviation")
# a controller of H3b at a gain of zero gives the open-loop run with the metronome
run_b0 <- simulate_run(m1, n_cycles = n_total, metronome = met,
                       controller = make_controller_h3b(w, kappa = 0, lin))
if (!identical(run_b0$events$asynchrony, run_free$events$asynchrony) ||
    !identical(run_b0$events$u, run_free$events$u)) {
  stop("the run of H3b at a gain of zero was expected to equal the open-loop run with the metronome")
}
if (!identical(run_free$events$duration, run_open$events$duration)) stop("the metronome was expected to leave the open-loop run unchanged")
# scales immediate and full_state: formulas of the correction
run_i <- simulate_run(m1, n_cycles = 20, metronome = met,
                      controller = make_controller_h3b(w, kappa = 0.05, lin, scale = "immediate"))
if (!isTRUE(all.equal(run_i$events$u_corr, -0.05 * run_i$events$deviation / lin$e_sc, tolerance = 1e-12))) {
  stop("u_corr of H3b differs from the formula of the immediate scale")
}
run_f <- simulate_run(m1, n_cycles = 20, metronome = met,
                      controller = make_controller_h3b(w, kappa = 0.05, lin, scale = "full_state"))
dx_f <- cbind(run_f$events$theta_post - lin$x_star[1], run_f$events$theta_dot_post - lin$x_star[2])
corr_f <- -0.05 * (run_f$events$deviation + as.numeric(dx_f %*% lin$a_vec)) / lin$e_sc
if (!isTRUE(all.equal(run_f$events$u_corr, corr_f, tolerance = 1e-12))) stop("u_corr of H3b differs from the formula of the full_state scale")
a_free <- run_free$events$asynchrony
guide_number("free_last", a_free[n_total], "%.2f", "asynchrony of the last contact of the open-loop run, in model time units")
guide_number("free_mean_below", 1000 * (lin$period - mean(run_free$events$duration)), "%.1f",
             "period of the metronome minus the mean step duration of the open-loop run, in 1e-3 time units")
if (mean(run_free$events$duration) >= lin$period) stop("the mean step duration of the open-loop run was expected to lie below the period")
if (abs(a_free[n_total] - n_total * (mean(run_free$events$duration) - lin$period)) > 1e-9) {
  stop("the last asynchrony was expected to equal the sum of the deviations of the step durations")
}
guide_number("b_min", min(ev_b$asynchrony), "%.2f", "smallest asynchrony of the run of H3b, in model time units")
guide_number("b_max", max(ev_b$asynchrony), "%.2f", "largest asynchrony of the run of H3b, in model time units")

## ---- hidden: figure-asynchrony ----
col <- guide_colors()
guide_figure("asynchrony", width_cm = 15.5, height_cm = 4.2, draw = function() {
  par(mar = c(3.4, 4.9, 0.5, 0.7), mgp = c(2.4, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  k <- seq_len(n_total)
  y_lim <- c(-0.8, 0.2)
  if (min(a_free, ev_b$asynchrony) < y_lim[1] || max(a_free, ev_b$asynchrony) > y_lim[2]) {
    stop("an asynchrony lies outside the vertical axis of the figure")
  }
  plot(k, a_free, type = "n", axes = FALSE, xlab = "Cycle of the model (step)",
       ylab = "", xlim = c(1, n_total), ylim = y_lim, xaxs = "i")
  # label of the vertical axis on two lines: the plot region is lower than the label is long
  mtext("Asynchrony", side = 2, line = 3.7, las = 0)
  mtext("(time units)", side = 2, line = 2.7, las = 0)
  abline(h = 0, col = col[["gray"]], lwd = 0.5)
  lines(k, a_free, col = col[["blue"]], lwd = 1.1)
  lines(k, ev_b$asynchrony, col = col[["vermilion"]], lwd = 1.1)
  axis(1, at = c(1, 50, 100, 150, 200, 250, 300, 320), col = col[["gray"]])
  ticks <- c(-0.8, -0.6, -0.4, -0.2, 0, 0.2)
  axis(2, at = ticks, labels = sprintf("%.1f", ticks), col = col[["gray"]])
  legend("bottomleft", legend = c("open loop (gain of zero)", expression("controller of H3b, " * kappa == 0.1)),
         col = col[c("blue", "vermilion")], lwd = 1.1, bty = "n", cex = 0.95, seg.len = 1.6)
})

## ---- linearized-loop ----
gain_scale(lin, "h3b", "static")
round(closed_loop_matrix(lin, "h3b", "static", gain = 0.1), 4)
closed_loop_radius(lin, "h3b", "static", gain = 0.1)
c(shift = stable_gain_limit(lin, "h3a", "shift"),
  deadbeat = stable_gain_limit(lin, "h3a", "deadbeat"),
  immediate = stable_gain_limit(lin, "h3b", "immediate"),
  static = stable_gain_limit(lin, "h3b", "static"),
  full_state = stable_gain_limit(lin, "h3b", "full_state"))
sapply(c(0, 0.05, 0.1, 0.2, 0.5), transient_cycles_loop, lin = lin,
       controller = "h3b", scale = "static")

## ---- hidden: numbers-linearized-loop ----
# first positive gain of the grid unstable: NA
if (!is.na(stable_gain_limit(lin, "h3b", "static", grid = c(0.7, 0.8)))) stop("a grid whose first gain is unstable was expected to give NA")
# meaning of a gain of 1 on each scale, in the linear model
m_dead <- closed_loop_matrix(lin, "h3a", "deadbeat", gain = 1)
if (max(abs(m_dead[1, ])) > 1e-9) stop("deadbeat scale: a gain of 1 was expected to cancel the row of the corrected coordinate")
m_shift <- closed_loop_matrix(lin, "h3a", "shift", gain = 1)
if (max(abs(m_shift[1, ] - (lin$j_map[1, ] - c(1, 0)))) > 1e-9) stop("shift scale: a gain of 1 was expected to subtract the deviation")
m_full <- closed_loop_matrix(lin, "h3b", "full_state", gain = 1)
if (max(abs(m_full[3, ])) > 1e-9) stop("full_state scale: a gain of 1 was expected to cancel the row of the asynchrony")
m_imm <- closed_loop_matrix(lin, "h3b", "immediate", gain = 1)
if (abs(m_imm[3, 3]) > 1e-9) stop("immediate scale: a gain of 1 was expected to cancel the direct term of the asynchrony")
# the static gain is the sum of the responses of the step duration to one input value
resp <- lin$e_sc
jp <- diag(2)
for (j in 1:400) {
  resp <- resp + sum(lin$a_vec * (jp %*% lin$b_vec))
  jp <- jp %*% lin$j_map
}
if (abs(resp - lin$static_gain) > 1e-8) stop("the static gain was expected to equal the sum of the responses of the step duration")
if (!identical(closed_loop_matrix(lin, "h3a", "deadbeat", gain = 0), lin$j_map)) stop("the matrix of H3a at a gain of zero was expected to equal J")
if (!identical(gain_scale(lin, "h3b", "full_state"), gain_scale(lin, "h3b", "immediate"))) stop("the factors of the scales full_state and immediate were expected to be equal")
if (!isTRUE(all.equal(gain_scale(lin, "h3a", "deadbeat"), lin$j_map[1, ] / lin$b_vec[1]))) stop("the factors of the deadbeat scale differ from the description of the text")
if (!isTRUE(all.equal(c(gain_scale(lin, "h3a", "shift"), gain_scale(lin, "h3b", "immediate"), gain_scale(lin, "h3b", "static")),
                      c(1 / lin$b_vec[1], 1 / lin$e_sc, 1 / lin$static_gain)))) {
  stop("the factors of the scales shift, immediate and static differ from the description of the text")
}
m_b <- closed_loop_matrix(lin, "h3b", "static", gain = 0.1)
if (!identical(dim(m_b), c(3L, 3L)) || !identical(m_b[1:2, 1:2], lin$j_map) || !identical(m_b[3, 1:2], lin$a_vec)) {
  stop("the matrix of H3b differs from the description of the text")
}
if (!isTRUE(all.equal(m_b[, 3], c(-0.1 * lin$b_vec / lin$static_gain, 1 - 0.1 * lin$e_sc / lin$static_gain)))) {
  stop("the last column of the matrix of H3b differs from the description of the text")
}
if (closed_loop_radius(lin, "h3b", "static", gain = 0.1) >= 1 || closed_loop_radius(lin, "h3a", "deadbeat", gain = 0.1) >= 1) {
  stop("the loops of the two runs of the example were expected to be stable")
}
guide_number("limit_deadbeat", stable_gain_limit(lin, "h3a", "deadbeat"), "%.2f", "largest stable gain of H3a, scale deadbeat, default grid")
guide_number("limit_immediate", stable_gain_limit(lin, "h3b", "immediate"), "%.2f", "largest stable gain of H3b, scale immediate, default grid")
guide_number("limit_static", stable_gain_limit(lin, "h3b", "static"), "%.2f", "largest stable gain of H3b, scale static, default grid")
guide_number("limit_full", stable_gain_limit(lin, "h3b", "full_state"), "%.2f", "largest stable gain of H3b, scale full_state, default grid")
if (abs(closed_loop_radius(lin, "h3b", "static", 0) - 1) > 1e-9) stop("the loop of H3b at a gain of zero was expected to have a spectral radius of 1")
if (abs(closed_loop_radius(lin, "h3a", "shift", 0) - max(Mod(lin$eig))) > 1e-12) stop("the loop of H3a at a gain of zero was expected to have the spectral radius of J")
# no unstable gain on the grid: the largest gain of the grid
if (!identical(stable_gain_limit(lin, "h3b", "static", grid = seq(0, 0.5, by = 0.1)), 0.5)) stop("a grid without an unstable gain was expected to give its largest gain")
# a grid without a positive gain: -Inf, with a warning of max()
no_gain <- tryCatch(stable_gain_limit(lin, "h3b", "static", grid = 0), warning = function(w) "warning")
if (!identical(no_gain, "warning")) stop("a grid without a positive gain was expected to give a warning")
# transient cycles under a controller
guide_number("radius_static_005", closed_loop_radius(lin, "h3b", "static", 0.05), "%.3f", "spectral radius of H3b, scale static, at a gain of 0.05")
guide_number("n_tr_005", transient_cycles_loop(lin, "h3b", "static", gain = 0.05), "%d", "transient cycles of H3b, scale static, gain 0.05")
guide_number("n_tr_020", transient_cycles_loop(lin, "h3b", "static", gain = 0.2), "%d", "transient cycles of H3b, scale static, gain 0.2")
if (transient_cycles_loop(lin, "h3b", "static", gain = 0.2, minimum = 0L) >= 20L) stop("at a gain of 0.2 the minimum of 20 cycles was expected to apply")
if (!identical(transient_cycles_loop(lin, "h3b", "static", gain = 0), transient_cycles(m1)) ||
    !identical(transient_cycles_loop(lin, "h3a", "deadbeat", gain = 0), transient_cycles(m1))) {
  stop("the transient at a gain of zero was expected to equal that of the open loop")
}
if (!identical(transient_cycles_loop(lin, "h3b", "static", gain = 0.1),
               transient_from_radius(closed_loop_radius(lin, "h3b", "static", 0.1)))) {
  stop("the transient at a positive gain was expected to follow from the spectral radius")
}
# at a gain of zero the argument scale is not evaluated, and at a positive gain an unknown
# scale stops the function with the message of closed_loop_matrix()
if (!identical(transient_cycles_loop(lin, "h3b", gain = 0), transient_cycles(m1))) stop("the transient at a gain of zero was expected not to need a scale")
if (!identical(tryCatch(transient_cycles_loop(lin, "h3b", "deadbeat", gain = 0.1), error = conditionMessage),
               "unknown scale of H3b: deadbeat")) stop("the transient at a positive gain was expected to stop on an unknown scale")
if (!identical(sapply(c(0, 0.05, 0.1, 0.2, 0.5), function(x) transient_cycles_loop(lin, "h3b", "static", gain = x)),
               c(20L, 52L, 26L, 20L, 36L))) stop("the transient cycles of the five gains differ from the values of the public tests")

## ---- hidden: figure-radius ----
gains <- seq(0, 1.2, by = 0.01)
radius_curve <- function(controller, scale) {
  vapply(gains, function(x) closed_loop_radius(lin, controller, scale, x), numeric(1))
}
curves_a <- list(shift = radius_curve("h3a", "shift"), deadbeat = radius_curve("h3a", "deadbeat"))
curves_b <- list(immediate = radius_curve("h3b", "immediate"), static = radius_curve("h3b", "static"),
                 full_state = radius_curve("h3b", "full_state"))
guide_number("deadbeat_min_radius", min(curves_a$deadbeat), "%.2f", "smallest spectral radius of H3a, scale deadbeat, over the gains 0 to 1.2 by 0.01")
guide_number("deadbeat_min_gain", gains[which.min(curves_a$deadbeat)], "%.2f", "gain of the smallest spectral radius of H3a, scale deadbeat")
guide_figure("radius", width_cm = 15.5, height_cm = 5.0, draw = function() {
  par(mfrow = c(1, 2), mar = c(3.4, 3.9, 1.8, 0.7), mgp = c(2.3, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  panel <- function(curves, xlab, tag, legend_at) {
    plot(gains, gains, type = "n", axes = FALSE, xlab = xlab, ylab = "Spectral radius",
         xlim = c(0, 1.2), ylim = c(0, 1.6), xaxs = "i", yaxs = "i")
    abline(h = 1, col = col[["gray"]], lty = 2, lwd = 0.8)
    for (i in seq_along(curves)) lines(gains, curves[[i]], col = col[[i]], lwd = 1.5)
    axis(1, at = seq(0, 1.2, by = 0.2), col = col[["gray"]])
    axis(2, at = seq(0, 1.5, by = 0.5), col = col[["gray"]])
    legend(legend_at, legend = paste0("\"", names(curves), "\""), col = col[seq_along(curves)],
           lwd = 1.5, bty = "n", cex = 0.95, seg.len = 1.6)
    title(main = tag, adj = 0, font.main = 1, cex.main = 1, line = 0.6)
  }
  panel(curves_a, expression("Gain " * italic(g)), "(a) Controller of H3a", "topleft")
  panel(curves_b, expression("Gain " * kappa), "(b) Controller of H3b", "bottomright")
})

## ---- trial ----
kappa <- 0.1
n_tr <- transient_cycles_loop(lin, "h3b", "static", gain = kappa)
draw <- draw_single_seed(m1, function(n, seed) {
  input_arfima(n, d = 0, sd = 0.002, seed = seed, n_transient = n_tr)
}, n = n_tr + n_keep)
seed_of <- function(a) seed_campaign(90000000, cell = 91, replicate = 1, a)
factory <- function(input) make_controller_h3b(input, kappa = kappa, lin)
trial <- run_trial(m1, draw, seed_of, n_transient = n_tr, n_keep = n_keep,
                   controller_factory = factory, metronome = met,
                   cell = 91, replicate = 1, keep_run = TRUE)
ev_t <- trial$run$events
identical(ev_t$u_exo, trial$run$input$values[n_tr + seq_len(n_keep)])
round(ev_t[1, c("cycle", "t_end", "beat", "asynchrony")], 4)

## ---- hidden: numbers-trial ----
guide_number("trial_n_transient", n_tr, "%d", "transient cycles of the trial of H3b")
guide_number("trial_seed", trial$seeds[1], "%d", "seed of attempt 0 of the trial")
guide_number("trial_beat1", ev_t$beat[1], "%.2f", "beat of the first kept cycle of the trial, in model time units")
guide_number("trial_first_kept", n_tr + 1L, "%d", "index, in the full run, of the first kept cycle of the trial")
if (trial$fell || trial$n_redraws != 0L || nrow(ev_t) != n_keep) stop("the trial was expected to complete at its first attempt")
if (!isTRUE(all.equal(ev_t$beat[1], (n_tr + 1L) * lin$period))) stop("the beat of the first kept cycle was expected to keep its value of the full run")
if (ev_t$cycle[1] != 1L || ev_t$t_start[1] != 0) stop("the kept cycles were expected to be numbered from 1 and to start at time zero")
if (!identical(trial$run$metronome_period, lin$period)) stop("the kept run was expected to record the period of the metronome")
if (is.na(trial$summary$alpha_asynchrony) || is.na(trial$summary$alpha_command) ||
    is.na(trial$summary$alpha_input)) {
  stop("the summary row of the closed-loop trial was expected to hold the three exponents of the series")
}

## ---- user-controller ----
make_controller_zone <- function(w, kappa, lin, zone) {
  function(k, info) {
    a <- info$asynchrony                # asynchrony of contact k - 1
    corr <- if (abs(a) > zone) kappa * a / lin$static_gain else 0
    list(u = w$values[k] - corr, u_exo = w$values[k], u_corr = -corr)
  }
}
ctrl_zone <- make_controller_zone(w, kappa = 0.1, lin, zone = 0.05)
run_zone <- simulate_run(m1, n_cycles = n_total, controller = ctrl_zone,
                         metronome = met)
run_zone$fell
round(mean(run_zone$events$u_corr == 0), 2)    # share of uncorrected cycles

## ---- hidden: numbers-user ----
guide_number("zone", 0.05, "%.2f", "dead zone of the controller defined by the user, in model time units")
guide_number("zone_share", 100 * mean(run_zone$events$u_corr == 0), "%.0f", "cycles without correction in the run of the controller defined by the user, in percent")
if (run_zone$fell || run_zone$n_cycles_done != n_total) stop("the run of the controller defined by the user was expected to complete")
if (!all(is.na(run_zone$events$deviation)) || anyNA(run_zone$events$u_corr)) stop("the columns of the run of the controller defined by the user differ from the description of the text")

## ---- hidden: checks-errors ----
# every message of the table of errors of the chapter is produced here; the chapter quotes it
message_of <- function(expr) {
  tryCatch({ expr; "no error" }, error = conditionMessage)
}
expect_message <- function(got, expected) {
  if (!identical(got, expected)) stop("message differs from the table of the chapter: ", got)
}
expect_message(message_of(simulate_run(m1, n_cycles = 5, controller = ctrl_b)),
               "the controller of H3b needs a metronome")
expect_message(message_of(simulate_run(m1, n_cycles = 5, controller = function(k, info) list(u = NA))),
               "controller returned a non-finite input at cycle 1")
expect_message(message_of(gain_scale(lin, "h3b", "deadbeat")), "unknown scale of H3b: deadbeat")
expect_message(message_of(transient_cycles_loop(lin, "h3b", "static", gain = 0.7)),
               "the loop is not stable (largest eigenvalue modulus 1.083369)")
short <- simaci_input(w$values[1:30], source = "short")
guide_number("short_n", length(short$values), "%d", "length of the series that is shorter than the run")
guide_number("short_cycles", 40, "%d", "cycles of the run with the short series")
guide_number("short_stop", length(short$values) + 1L, "%d", "cycle at which the run stops with a series of 30 values")
if (message_of(simulate_run(m1, n_cycles = 40, controller = make_controller_h3a(short, g = 0.1, lin))) !=
    sprintf("controller returned a non-finite input at cycle %d", length(short$values) + 1L)) {
  stop("a short series was expected to stop the run at its first cycle without a value")
}
if (message_of(simulate_run(m1, n_cycles = 5)) != "an open-loop run needs an input") stop("message of a run without input and without controller")
if (message_of(transient_cycles_loop(lin, "h3b", "static", gain = -0.1)) != "gain must be one value >= 0" ||
    message_of(transient_cycles_loop(lin, "h3b", "static", gain = c(0.1, 0.2))) != "gain must be one value >= 0") {
  stop("message of transient_cycles_loop() for a gain that is not one value of zero or more")
}
if (message_of(simulate_run(m1, n_cycles = 2, controller = function(k, info) list(v = 0))) !=
    "controller returned a non-finite input at cycle 1") stop("a list without u was expected to stop the run")
if (message_of(simulate_run(m1, n_cycles = 2, controller = function(k, info) list(u = c(0, 0)))) !=
    "controller returned a non-finite input at cycle 1") stop("an input of length 2 was expected to stop the run")
if (message_of(gain_scale(lin, "h3a", "static")) != "unknown scale of H3a: static") stop("message of gain_scale() for H3a")
if (message_of(closed_loop_matrix(lin, "h3a", "static", 0.1)) != "unknown scale of H3a: static") stop("message of closed_loop_matrix() for H3a")
if (message_of(closed_loop_matrix(lin, "h3b", "deadbeat", 0.1)) != "unknown scale of H3b: deadbeat") stop("message of closed_loop_matrix() for H3b")
if (message_of(make_controller_h3a(w, 0.1, lin, corrected_index = 3)) != "subscript out of bounds") stop("message of an index outside the reduced state")
if (!is.na(gain_scale(lin, "h3a", "shift", corrected_index = 3))) stop("gain_scale() was expected to return NA for an index outside the reduced state")
# post-generation fix by Claude Fable 5.2, 2026-11-14: check of a statement added to the chapter
# (fix 09-3 of review/pass2_fixes.json)
if (message_of(gain_scale(lin, "h3a", "deadbeat", corrected_index = 3)) != "subscript out of bounds") {
  stop("gain_scale() was expected to stop on the deadbeat scale for an index outside the reduced state")
}
# an unknown controller stops the five functions of the linearized loop with the error of match.arg()
unknown <- c(message_of(gain_scale(lin, "h3c", "static")),
             message_of(closed_loop_matrix(lin, "h3c", "static", 0.1)),
             message_of(closed_loop_radius(lin, "h3c", "static", 0.1)),
             message_of(stable_gain_limit(lin, "h3c", "static")),
             message_of(transient_cycles_loop(lin, "h3c", "static", gain = 0.1)))
if (!all(grepl("'arg' should be one of", unknown, fixed = TRUE))) stop("an unknown controller was expected to stop the five functions")
if (!grepl("'arg' should be one of", message_of(make_controller_h3a(w, 0.1, lin, scale = "static")), fixed = TRUE) ||
    !grepl("'arg' should be one of", message_of(make_controller_h3b(w, 0.1, lin, scale = "shift")), fixed = TRUE)) {
  stop("an unknown scale was expected to stop the two functions that build a controller")
}
