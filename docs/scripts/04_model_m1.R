# Title: Worked example of the chapter "Model M1, the simplest walker" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: builds model M1 of walkbench 0.7.0 with model_m1(), reads the members of the
#   model object, computes the fixed point of the cycle map and one cycle with and without an
#   input, simulates an unperturbed run on the limit cycle, converts a sample from the
#   continuous leg coordinates to the stance-leg coordinates, and simulates a run that ends in
#   a fall. The example is a demonstration computed for the guide. Its only seed, 90,004,001,
#   lies in the block 90,000,000 to 92,099,999 of the guide (cell 40, first seed given directly
#   to a generator), which check_seed_plan() finds apart from the seed blocks of the SIM-ACI
#   campaigns. The blocks whose label starts with "hidden:" write the signatures of the
#   functions, register the numbers quoted in the chapter, check the statements of the text
#   (members of the model object, error messages, search of the fixed point at other slopes)
#   and draw its figure; the other blocks are printed in the chapter with their output.
# Dependencies: walkbench 0.7.0 (loaded by the driver), stats, graphics, grDevices
# Input: none
# Output: generated/04_model_m1/: snippets/*.txt, output/*.txt, signatures/*.txt, numbers.tex,
#   numbers.csv, fig_limit_cycle.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/04_model_m1.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
guide_signature("model_m1")
guide_signature("list_models")

## ---- model ----
m1 <- model_m1()
unlist(m1$params)
ip <- m1$injection_points
data.frame(variant = sapply(ip, function(p) p$variant),
           unit = sapply(ip, function(p) p$value_unit),
           linearizable = sapply(ip, function(p) p$linearizable))
names(list_models())

## ---- hidden: numbers-model ----
members <- c("id", "label", "version", "params", "time_unit", "cycle_unit",
             "cycles_per_stride", "state_names", "reduced_state_names", "injection_points",
             "default_injection", "fixed_point", "post_contact_state", "cycle_map", "flow",
             "rhs", "coord_maps", "simulate", "normalize_cycles", "corrected_index",
             "estimation_settings", "fall_rule")
if (!identical(names(m1), members)) stop("the members of the model object differ from the table of the chapter")
if (!identical(names(ip$foot_strike_timing),
               c("variant", "value_unit", "default", "linearizable", "description"))) {
  stop("the fields of an injection point differ from the text of the chapter")
}
if (!inherits(m1, "simaci_model") || !identical(m1$state_names, c("thG", "thGd", "phG", "phGd")) ||
    !identical(m1$reduced_state_names, c("theta", "theta_dot"))) {
  stop("the class or the coordinate names of the model object differ from the chapter")
}
if (!identical(m1$default_injection, "foot_strike_timing") || m1$cycle_unit != "step" ||
    m1$cycles_per_stride != 2L || m1$corrected_index != 1L || m1$version != "0.1.0") {
  stop("a member of the model object differs from the table of the chapter")
}
if (!identical(vapply(list_models(), function(m) m$id, ""), c(M1 = "M1", M2 = "M2"))) {
  stop("list_models() was expected to return M1 and M2")
}
is_fun <- vapply(m1, is.function, logical(1))
if (!identical(names(m1)[is_fun], c("fixed_point", "post_contact_state", "cycle_map", "flow",
                                    "rhs", "simulate", "normalize_cycles"))) {
  stop("the function members of the model object differ from the table of the chapter")
}
if (!identical(names(formals(m1$simulate)),
               c("input", "n_cycles", "y0", "controller", "metronome", "injection"))) {
  stop("the arguments of simulate() differ from the text of the chapter")
}
guide_number("n_members", length(members), "%d", "members of the model object")
guide_number("n_functions", sum(is_fun), "%d", "members of the model object that are functions")
guide_number("n_data", sum(!is_fun), "%d", "members of the model object that hold data")
if (length(ip) != 5L || sum(vapply(ip, function(p) p$linearizable, logical(1))) != 3L) {
  stop("five injection points, three of them with a reduced cycle map, were expected")
}
if (!identical(m1$params, list(gamma = 0.009, dt = 0.03, t_min = 1))) {
  stop("the default arguments of model_m1() differ from the table of the chapter")
}
guide_number("time_unit_s", sqrt(1 / 9.81), "%.3f", "one time unit sqrt(L/g) in seconds, for L = 1 m and g = 9.81 m/s^2")

## ---- fixed-point ----
fp <- m1$fixed_point()
fp$x_star
fp$period
fp$eig
Mod(fp$eig)
cm <- m1$cycle_map(fp$x_star, u = 0)
max(abs(cm$x_next - fp$x_star))

## ---- hidden: numbers-fixed-point ----
if (!identical(names(fp), c("x_star", "period", "y0", "eig"))) stop("the fields of the fixed point differ from the text of the chapter")
if (max(abs(m1$post_contact_state(fp$x_star) - fp$y0)) > 0) stop("y0 was expected to equal post_contact_state(x_star)")
if (max(Mod(fp$eig)) >= 1) stop("the fixed point of M1 was expected to be stable")
if (max(abs(cm$x_next - fp$x_star)) > 1e-12) stop("the cycle map was expected to return the fixed point")
guide_number("theta_star", fp$x_star[1], "%.4f", "stance angle after contact at the fixed point of M1, in rad")
guide_number("theta_dot_star", fp$x_star[2], "%.4f", "angular velocity of the stance leg after contact at the fixed point of M1, in rad per time unit")
guide_number("t_star", fp$period, "%.4f", "step duration on the limit cycle of M1, in time units")
guide_number("t_star_s", fp$period * sqrt(1 / 9.81), "%.2f", "step duration on the limit cycle of M1 in seconds, for L = 1 m")
guide_number("eig_re", Re(fp$eig[1]), "%.3f", "real part of the eigenvalues of the step map of M1")
guide_number("eig_im", abs(Im(fp$eig[1])), "%.3f", "absolute imaginary part of the eigenvalues of the step map of M1")
guide_number("eig_mod", Mod(fp$eig[1]), "%.3f", "modulus of the eigenvalues of the step map of M1, from fixed_point()")
guide_number("fp_residual_e15", 1e15 * max(abs(cm$x_next - fp$x_star)), "%.0f", "largest difference between cycle_map(x_star, 0) and x_star, in 1e-15")
# crossings of the foot-strike condition phi - 2 theta = 0 inside a step of the limit cycle
ell_fine <- seq(0, fp$period, length.out = 4001)
y_fine <- m1$flow(fp$x_star, 0, ell_fine)
g_fine <- y_fine[, 3] - 2 * y_fine[, 1]
inside <- which(diff(sign(g_fine[-1])) != 0) + 1L
if (length(inside) != 1L || g_fine[inside] <= 0) stop("one downward crossing was expected inside the step")
guide_number("scuff_time", ell_fine[inside], "%.2f", "time of the crossing of phi - 2 theta = 0 at mid-swing on the limit cycle, in time units")

## ---- cycle-map ----
one_cycle <- function(u, injection) {
  cm <- m1$cycle_map(fp$x_star, u, injection)
  c(theta = cm$x_next[[1]], theta_dot = cm$x_next[[2]], duration = cm$duration)
}
rbind(no_input = one_cycle(0, "foot_strike_timing"),
      timing = one_cycle(0.002, "foot_strike_timing"),
      full = one_cycle(0.002, "foot_strike_full"),
      slope = one_cycle(0.002, "slope"))

## ---- hidden: numbers-cycle-map ----
cm_tab <- rbind(no_input = one_cycle(0, "foot_strike_timing"),
                timing = one_cycle(0.002, "foot_strike_timing"),
                full = one_cycle(0.002, "foot_strike_full"),
                slope = one_cycle(0.002, "slope"))
if (!isTRUE(all.equal(cm_tab["timing", "duration"], cm_tab["full", "duration"], tolerance = 1e-12))) {
  stop("the same step duration was expected at foot_strike_timing and foot_strike_full")
}
guide_number("cm_u", 1000 * 0.002, "%.0f", "input of the cycle map example, in 1e-3 rad")
guide_number("cm_dur_timing", 1000 * (cm_tab["timing", "duration"] - fp$period), "%.2f", "change of the step duration for an input of 0.002 rad at foot_strike_timing, in 1e-3 time units")
guide_number("cm_dur_slope", 1000 * (cm_tab["slope", "duration"] - fp$period), "%.1f", "change of the step duration for an input of 0.002 rad at slope, in 1e-3 time units")
guide_number("cm_theta_timing", 1000 * (cm_tab["timing", "theta"] - fp$x_star[1]), "%.2f", "change of the stance angle after contact for an input of 0.002 rad at foot_strike_timing, in 1e-3 rad")
guide_number("cm_theta_full", 1e6 * abs(cm_tab["full", "theta"] - fp$x_star[1]), "%.1f", "absolute change of the stance angle after contact for an input of 0.002 rad at foot_strike_full, in 1e-6 rad")

## ---- limit-cycle ----
n <- 100
zeros <- simaci_input(numeric(n), source = "zero")
run0 <- simulate_run(m1, zeros, n_cycles = n)
run0$fell
max(abs(run0$events$duration - fp$period))
range(table(run0$samples[, "cycle"]))

## ---- hidden: numbers-limit-cycle ----
dev0 <- max(abs(run0$events$duration - fp$period))
if (run0$fell || dev0 > 1e-8) stop("the unperturbed run was expected to stay on the limit cycle")
if (max(abs(diff(run0$samples[, "t"]) - m1$params$dt)) > 1e-9) stop("the samples were expected on one grid of step dt")
guide_number("n_cycles0", n, "%d", "cycles of the unperturbed run")
guide_number("n_samples0", format(nrow(run0$samples), big.mark = ","), "%s", "samples of the unperturbed run")
guide_number("dur_dev_e9", 1e9 * dev0, "%.1f", "largest deviation of the step duration from T* in the unperturbed run, in 1e-9 time units")
guide_number("samples_min", min(table(run0$samples[, "cycle"])), "%d", "smallest number of samples in a cycle of the unperturbed run")
guide_number("samples_max", max(table(run0$samples[, "cycle"])), "%d", "largest number of samples in a cycle of the unperturbed run")
guide_number("first_sample_t", run0$samples[1, "t"], "%.2f", "time of the first sample of a run, in time units")

## ---- coordinates ----
s <- run0$samples[run0$samples[, "cycle"] == 2, ][1, ]
round(s, 4)
round(solve(m1$coord_maps[[2]], s[m1$state_names]), 4)

## ---- hidden: numbers-coordinates ----
w <- solve(m1$coord_maps[[2]], s[m1$state_names])
if (run0$events$leg[2] != 2L) stop("leg 2 was expected to be the stance leg of cycle 2")
if (!identical(m1$coord_maps[[1]], diag(c(1, 1, -1, -1)))) stop("the map of stance leg 1 differs from the text of the chapter")
guide_number("s2_t", s[["t"]], "%.2f", "time of the first sample of cycle 2, in time units")
guide_number("s2_theta", w[1], "%.4f", "stance angle at the first sample of cycle 2, in rad")
b_end <- run0$bounds[[1]]$end[1, ]
b_start <- run0$bounds[[2]]$start[1, ]
if (max(abs(b_end[c("thG", "phG")] - b_start[c("thG", "phG")])) > 1e-12) stop("the leg angles were expected to be continuous at the foot strike")
guide_number("thgd_before", b_end[["thGd"]], "%.3f", "thGd just before the first foot strike of the limit cycle, in rad per time unit")
guide_number("thgd_after", b_start[["thGd"]], "%.3f", "thGd just after the first foot strike of the limit cycle, in rad per time unit")

## ---- fall ----
u_big <- input_iid_uniform(40, rho = 0.12, seed = 90004001)
run_fall <- simulate_run(m1, u_big, n_cycles = 40)
str(run_fall[c("fell", "fall_cycle", "n_cycles_done")])
tabulate(run_fall$samples[, "cycle"])
last <- tail(run_fall$samples, 1)[1, ]
round(solve(m1$coord_maps[[2]], last[m1$state_names]), 3)

## ---- hidden: numbers-fall ----
if (!run_fall$fell || run_fall$fall_cycle %% 2L != 0L) stop("a fall in a cycle whose stance leg is leg 2 was expected")
if (nrow(run_fall$events) != run_fall$n_cycles_done || length(run_fall$bounds) != run_fall$n_cycles_done) {
  stop("the table of events was expected to hold the completed cycles only")
}
w_last <- solve(m1$coord_maps[[2]], last[m1$state_names])
if (w_last[1] > -1.1) stop("the stance angle was expected near the bound of -1.2 rad at the last sample")
guide_number("fall_rho", u_big$params$rho, "%.2f", "amplitude rho of the uniform input of the run that falls, in rad")
guide_number("fall_n", 40, "%d", "cycles requested in the run that falls")
guide_number("fall_cycle", run_fall$fall_cycle, "%d", "cycle of the fall")
guide_number("fall_done", run_fall$n_cycles_done, "%d", "completed cycles of the run that falls")
guide_number("fall_samples_last", sum(run_fall$samples[, "cycle"] == run_fall$fall_cycle), "%d", "samples of the cycle of the fall")
guide_number("fall_theta_last", w_last[1], "%.3f", "stance angle at the last sample of the run that falls, in rad")
trim_msg <- tryCatch({ trim_run(run_fall, 2L); "no error" }, error = conditionMessage)
if (trim_msg != "a run that fell is not trimmed") stop("the message of trim_run() differs from the text of the chapter")

## ---- hidden: errors ----
# the messages of the table of errors of the chapter, each produced by a call
message_of <- function(expr) {
  tryCatch({ expr; "no error" }, error = conditionMessage)
}
expect_message <- function(found, expected) {
  if (!identical(unname(found), expected)) {
    stop("a message differs from the table of the chapter: ", found)
  }
}
expect_message(message_of(m1$cycle_map(fp$x_star, 0.002, "terrain")),
               "no reduced cycle map for the injection point terrain")
expect_message(message_of(m1$cycle_map(fp$x_star, 0.002, "foot_strike_geometric")),
               "no reduced cycle map for the injection point foot_strike_geometric")
unknown <- "unknown injection point of M1: floor_height"
expect_message(message_of(m1$cycle_map(fp$x_star, 0.002, "floor_height")), unknown)
expect_message(message_of(m1$flow(fp$x_star, 0, c(0, 1), "floor_height")), unknown)
expect_message(message_of(simulate_run(m1, u_big, n_cycles = 40, injection = "floor_height")),
               unknown)
# a cycle that ends in a fall, at the three injection points with a reduced cycle map
fall_msg <- vapply(c("foot_strike_timing", "foot_strike_full", "slope"), function(p) {
  message_of(m1$cycle_map(c(0.2, -0.05), 0, p))
}, character(1))
expect_message(fall_msg[["foot_strike_timing"]], "invalid argument to unary operator")
expect_message(fall_msg[["foot_strike_full"]], "non-numeric argument to mathematical function")
expect_message(fall_msg[["slope"]], "non-numeric argument to mathematical function")
expect_message(message_of(model_m1(gamma = 0.015)$fixed_point()),
               "step map failed during Newton iteration")
# search of the fixed point at other slopes
slopes <- seq(0.002, 0.012, by = 0.002)
stable <- vapply(slopes, function(g) {
  m <- model_m1(gamma = g)
  f <- m$fixed_point()
  max(Mod(f$eig)) < 1 && max(abs(m$cycle_map(f$x_star, 0)$x_next - f$x_star)) < 1e-12
}, logical(1))
if (!all(stable)) stop("a stable fixed point was expected for the slopes 0.002 to 0.012 rad")
guide_number("slope_ok_min", min(slopes), "%.3f", "smallest slope of the check of the fixed point search, in rad")
guide_number("slope_ok_max", max(slopes), "%.3f", "largest slope of the check of the fixed point search, in rad")
guide_number("slope_ok_step", 0.002, "%.3f", "step of the slopes of the check of the fixed point search, in rad")
fp_steep <- model_m1(gamma = 0.02)$fixed_point()
if (max(Mod(fp_steep$eig)) <= 1) stop("an unstable fixed point was expected at a slope of 0.02 rad")
guide_number("slope_steep", 0.02, "%.2f", "slope of the example of an unstable fixed point, in rad")
guide_number("slope_steep_eig", max(Mod(fp_steep$eig)), "%.2f", "largest eigenvalue modulus of the fixed point returned at a slope of 0.02 rad")
guide_number("slope_fail", 0.015, "%.3f", "slope at which the search of the fixed point stops, in rad")
# an earliest time of foot strike above the step duration: the first step ends in a fall
run_late <- simulate_run(model_m1(t_min = 5), simaci_input(numeric(4), source = "zero"),
                         n_cycles = 4)
if (!run_late$fell || run_late$fall_cycle != 1L) stop("a fall in cycle 1 was expected with t_min = 5")
# the argument t_min does not enter the search of the fixed point
if (!identical(model_m1(t_min = 0.5)$fixed_point()$x_star, fp$x_star)) {
  stop("the fixed point was expected to be the same for another value of t_min")
}
# one value of 0.002 at the injection point terrain, where the unit is the leg length
u_one <- simaci_input(replace(numeric(8), 2, 0.002), source = "single value",
                      value_unit = "leg length")
run_terrain <- simulate_run(m1, u_one, n_cycles = 8, injection = "terrain")
run_timing <- simulate_run(m1, u_one, n_cycles = 8)
if (!run_terrain$fell || run_timing$fell) stop("a fall was expected at terrain and none at foot_strike_timing")
guide_number("terrain_u", 1000 * 0.002, "%.0f", "single input value of the unit example, in 1e-3 rad or 1e-3 leg length")
guide_number("terrain_fall_after", run_terrain$fall_cycle - 2L, "%d", "cycles between the input value at terrain and the fall")

## ---- hidden: figure-limit-cycle ----
col <- guide_colors()
# curves of cycle k, from its start to its end, in both coordinate systems
cycle_curves <- function(run, k) {
  smp <- run$samples[run$samples[, "cycle"] == k, , drop = FALSE]
  cont <- rbind(run$bounds[[k]]$start, smp[, m1$state_names], run$bounds[[k]]$end)
  list(t = c(run$events$t_start[k], smp[, "t"], run$events$t_end[k]), cont = cont,
       stance = t(solve(m1$coord_maps[[run$events$leg[k]]], t(cont))))
}
guide_figure("limit_cycle", width_cm = 15.5, height_cm = 8.0, draw = function() {
  cur <- lapply(1:2, function(k) cycle_curves(run0, k))
  strikes <- run0$events$t_end[1:2]
  # one panel: two coordinates of one system over the two cycles, with direct labels
  panel <- function(system, columns, tag, labels, label_x, label_y, ylab, bottom) {
    par(mar = c(0.9, if (nzchar(ylab)) 5.4 else 3.2, 1.8, 0.5))
    plot(NA, xlim = c(0, strikes[2] + 0.12), ylim = c(-0.5, 0.5), axes = FALSE, xlab = "",
         ylab = "", xaxs = "i")
    abline(h = 0, col = col[["light"]], lwd = 0.6)
    abline(v = strikes, col = col[["gray"]], lwd = 0.6, lty = 2)
    hues <- c(col[["blue"]], col[["vermilion"]])
    for (cc in cur) {
      for (j in 1:2) lines(cc$t, cc[[system]][, columns[j]], col = hues[j], lwd = 1.4)
    }
    for (j in 1:2) text(label_x[j], label_y[j], labels[[j]], col = hues[j], cex = 0.95)
    axis(2, at = seq(-0.4, 0.4, by = 0.2), col = col[["gray"]])
    if (bottom) axis(1, at = 0:7, col = col[["gray"]])
    if (nzchar(ylab)) mtext(ylab, side = 2, line = 2.9, las = 0)
    title(main = tag, adj = 0, font.main = 1, cex.main = 1, line = 0.5)
  }
  # the left column is wider by the 2.2 lines of its axis label (one line is 0.381 cm)
  layout(matrix(1:4, 2, 2, byrow = TRUE), widths = c(8.17, 7.33))
  par(oma = c(3.0, 0, 0.2, 0), mgp = c(2.4, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  panel("stance", c(1, 3), "(a) Stance-leg coordinates",
        list(expression(theta), expression(phi)), c(1.0, 1.0), c(0.15, 0.35),
        "Angle (rad)", FALSE)
  text(strikes[1] - 0.1, -0.46, "foot strike", adj = 1, cex = 0.9, col = col[["gray"]])
  panel("cont", c(1, 3), "(b) Continuous leg coordinates",
        list("thG", "phG"), c(1.0, 1.8), c(0.15, -0.3), "", FALSE)
  panel("stance", c(2, 4), "(c) Stance-leg coordinates",
        list(expression(d * theta / d * italic(t)), expression(d * phi / d * italic(t))),
        c(1.95, 0.6), c(0.06, -0.32), "Angular velocity\n(rad per time unit)", TRUE)
  panel("cont", c(2, 4), "(d) Continuous leg coordinates",
        list("thGd", "phGd"), c(1.0, 0.75), c(-0.02, 0.4), "", TRUE)
  mtext("Time from the start of the run (time units)", side = 1, outer = TRUE, line = 1.8)
})
