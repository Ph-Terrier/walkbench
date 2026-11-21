# Title: Worked example of the chapter "Model M2, the walker with arced feet and a hip spring"
#   of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: builds model M2 of walkbench 0.7.0 with its default parameters, prints its fixed
#   point and compares it with model M1 at the same slope (fixed point, and a run with the same
#   floor-height input). Reaches the configurations r_f = 0.3 and r_f = 0.3 with k = 2 by
#   continuation, with the fixed point of each configuration as the initial guess of the next
#   one, and shows that a direct call with r_f = 0.3 and the default guess stops with an
#   error. Computes the limit cycles of three configurations, a run with a floor-height input,
#   the two impact maps after one step of the floor, and a fall after a step up. The example is
#   a demonstration computed for the guide; it stays at moderate parameter values (r_f up to
#   0.3, k up to 2). Its seeds lie in the block of the guide: 90,005,001 (input, cell 50) and
#   90,005,101 (random states of the checks, cell 51). The blocks whose label starts with the
#   word hidden write the signature of model_m2(), check the statements of the chapter (the
#   equations typeset in the chapter against the code of the package, the relation to the
#   equations printed by Hobbelen and Wisse 2007, the geometry of the impact map, an unstable
#   cycle of the simplest walker at slope 0.02, a value of t_min above the step duration),
#   register the numbers quoted in the chapter and draw its figure; the other blocks are
#   printed in the chapter with their output. The script stops when a check fails.
# Dependencies: walkbench 0.7.0 (loaded by the driver), stats, graphics, grDevices, utils
# Input: none
# Output: generated/05_model_m2/: snippets/*.txt, output/*.txt, signatures/*.txt, numbers.tex,
#   numbers.csv, fig_limit_cycles.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/05_model_m2.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
library(utils)
# straight quotation marks in the messages of R, whatever the locale
options(useFancyQuotes = FALSE)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
guide_signature("model_m2")
# largest power of ten above a small difference: 3.2e-11 gives 10, read as "below 1e-10"
bound_exp <- function(x) as.integer(-ceiling(log10(max(x, .Machine$double.xmin))))

## ---- default-model ----
m2 <- model_m2()
fp <- m2$fixed_point()
round(setNames(fp$x_star, m2$reduced_state_names), 4)
round(c(period = fp$period, step_length = unname(fp$step_length)), 4)
signif(Mod(fp$eig), 4)

## ---- hidden: numbers-default ----
if (!identical(names(fp$x_star), c("1", "2", "3", "2"))) {
  stop("the names of x_star were expected to be 1, 2, 3, 2")
}
if (!identical(m2$estimation_settings, settings_m2())) {
  stop("the estimation settings of M2 were expected to be those of settings_m2()")
}
# members named in the table of the chapter
if (!identical(names(m2$injection_points), "floor_height") ||
    !identical(m2$injection_points$floor_height$value_unit, "leg length") ||
    !identical(m2$default_injection, "floor_height") ||
    !identical(names(m2$params), c("gamma", "k", "r_f", "dt", "t_min", "impact")) ||
    !identical(m2$reduced_state_names, c("theta", "theta_dot", "phi", "phi_dot")) ||
    !identical(names(fp), c("x_star", "period", "y0", "eig", "step_length")) ||
    !identical(m2$post_contact_state(fp$x_star), fp$x_star) ||
    !identical(model_m1()$params$t_min, 1)) {
  stop("a member of the model object differs from the table of the chapter")
}
if (!grepl("1.2 rad", m2$fall_rule, fixed = TRUE) || !grepl("15 time units", m2$fall_rule, fixed = TRUE)) {
  stop("the fall rule of M2 was expected to name 1.2 rad and 15 time units")
}
if (abs(unname(fp$step_length) - 2 * sin(fp$x_star[1])) > 1e-12) {
  stop("the step length was expected to equal 2 sin(theta*) at r_f = 0")
}
guide_number("gamma", m2$params$gamma, "%.3f", "default slope of M2, in rad")
guide_number("t_min", m2$params$t_min, "%.2f", "default t_min of M2, in time units")
guide_number("theta_star", fp$x_star[1], "%.4f", "theta at the fixed point of the default M2, in rad")
guide_number("phi_star", fp$x_star[3], "%.4f", "phi at the fixed point of the default M2, in rad")
guide_number("period", fp$period, "%.4f", "step duration on the limit cycle of the default M2, in time units")
guide_number("time_unit_s", sqrt(1 / 9.81), "%.2f", "time unit sqrt(L/g) for L = 1 m and g = 9.81 m/s2, in s")
guide_number("period_s", fp$period * sqrt(1 / 9.81), "%.2f", "step duration of the default M2 for L = 1 m, in s")
guide_number("step_length", fp$step_length, "%.4f", "step length of the default M2, in leg lengths")
guide_number("max_modulus", max(Mod(fp$eig)), "%.4f", "largest eigenvalue modulus of the cycle map of the default M2, fixed_point()$eig")
guide_number("small_eig_exp", bound_exp(sort(Mod(fp$eig))[2]), "%d", "the two smallest eigenvalue moduli of the default M2 lie below ten to minus this number")

## ---- m1-agreement ----
m1 <- model_m1(gamma = 0.004)
fp1 <- m1$fixed_point()
max(abs(fp$x_star - m1$post_contact_state(fp1$x_star)))
abs(fp$period - fp1$period)
round(c(M2 = max(Mod(fp$eig)), M1 = max(Mod(fp1$eig))), 4)
u <- input_iid_uniform(40, rho = 2e-4, seed = 90005001)
u$value_unit <- "leg length"
run_m2 <- simulate_run(m2, u, n_cycles = 40)
run_m1 <- simulate_run(m1, u, n_cycles = 40, injection = "terrain")
max(abs(run_m2$samples - run_m1$samples))

## ---- hidden: numbers-m1 ----
if (run_m2$fell || run_m1$fell) stop("the two runs of the comparison were expected to complete")
gap_fp <- max(abs(fp$x_star - m1$post_contact_state(fp1$x_star)))
gap_run <- max(abs(run_m2$samples - run_m1$samples))
if (gap_fp > 1e-9 || abs(fp$period - fp1$period) > 1e-8 || gap_run > 1e-8) {
  stop("M2 with r_f = 0 and k = 0 was expected to agree with M1")
}
guide_number("m1_fp_exp", bound_exp(gap_fp), "%d", "the fixed points of M2 (r_f = 0, k = 0) and M1 differ by less than ten to minus this number")
guide_number("m1_period_exp", bound_exp(abs(fp$period - fp1$period)), "%d", "the periods of M2 (r_f = 0, k = 0) and M1 differ by less than ten to minus this number")
guide_number("m1_run_exp", bound_exp(gap_run), "%d", "the samples of the runs of M2 and of M1 (terrain) differ by less than ten to minus this number")
guide_number("m1_modulus", max(Mod(fp1$eig)), "%.4f", "largest eigenvalue modulus of M1 at slope 0.004, fixed_point()$eig")
lin_m2 <- linearize(m2)
lin_m1 <- linearize(m1)
guide_number("lin_modulus_m2", max(Mod(lin_m2$eig)), "%.4f", "largest eigenvalue modulus of the default M2, central differences of linearize()")
guide_number("lin_modulus_m1", max(Mod(lin_m1$eig)), "%.4f", "largest eigenvalue modulus of M1 at slope 0.004, central differences of linearize()")
if (abs(max(Mod(lin_m2$eig)) - max(Mod(lin_m1$eig))) > 1e-4) {
  stop("the eigenvalue moduli of M2 and M1 from linearize() were expected to agree to 1e-4")
}
guide_number("run_n", run_m2$n_cycles_done, "%d", "cycles of the runs with a floor-height input")
guide_number("rho_e4", 1e4 * u$params$rho, "%.0f", "rho of the floor-height input, in 1e-4 leg length")
guide_number("u_bound_mm", 1000 * u$params$rho / 2, "%.1f", "bound rho / 2 of the floor-height input for a leg of 1 m, in mm")

## ---- direct-call ----
message_of <- function(expr) {
  tryCatch({ expr; "no error" }, error = conditionMessage)
}
m_far <- model_m2(r_f = 0.3)        # default guess; the search has not run yet
message_of(m_far$fixed_point())

## ---- continuation ----
row_of <- function(m) {
  f <- m$fixed_point()
  data.frame(r_f = m$params$r_f, k = m$params$k, period = f$period,
             step_length = unname(f$step_length),
             max_modulus = max(Mod(f$eig)))
}
x <- fp$x_star
path <- row_of(m2)
for (r_f in c(0.1, 0.2, 0.3)) {
  m <- model_m2(r_f = r_f, x_init = x)       # guess: the previous fixed point
  x <- m$fixed_point()$x_star
  path <- rbind(path, row_of(m))
}
m2_r <- m                                     # r_f = 0.3, k = 0
for (k in c(0.5, 1, 1.5, 2)) {
  m <- model_m2(r_f = 0.3, k = k, x_init = x)
  x <- m$fixed_point()$x_star
  path <- rbind(path, row_of(m))
}
m2_rk <- m                                    # r_f = 0.3, k = 2
print(round(path, 4), row.names = FALSE)

## ---- hidden: numbers-continuation ----
if (message_of(m_far$fixed_point()) != "fixed point of M2 not found") {
  stop("the direct call with r_f = 0.3 was expected to stop with the error of the search")
}
# default guess of the search (constants of fixed_point() in R/model_m2.R): from this state,
# the walker with r_f = 0.3 falls in its first step
g <- m2$params$gamma
th0 <- 0.970956 * g^(1 / 3) - 0.270837 * g
thd0 <- -1.045203 * 0.970956 * g^(1 / 3) + (1.045203 * 0.270837 + 1.062895) * g
guess <- c(th0, thd0, 2 * th0, thd0 * (1 - cos(2 * th0)))
if (message_of(m_far$cycle_map(guess, 0)) != "fall during cycle_map()") {
  stop("the walker with r_f = 0.3 was expected to fall from the default guess")
}
if (message_of(m2$cycle_map(guess, 0)) != "no error") {
  stop("the default walker was expected to complete a step from the default guess")
}
if (any(path$max_modulus >= 1)) stop("the cycles of the path were expected to be stable")
if (any(diff(path$step_length[1:4]) <= 0) || any(diff(path$step_length[4:8]) >= 0) ||
    any(diff(path$period[4:8]) >= 0)) {
  stop("the step length was expected to rise with r_f and to fall with k, like the period")
}
if (any(diff(path$step_length / path$period) <= 0)) {
  stop("the speed was expected to rise along the path")
}
# one step of 0.3 in r_f, from the fixed point of the default model, is too large
if (message_of(model_m2(r_f = 0.3, x_init = fp$x_star)$fixed_point()) !=
    "fixed point of M2 not found") {
  stop("a single step of 0.3 in r_f was expected to stop with the error of the search")
}
guide_number("r_far", m_far$params$r_f, "%.1f", "foot radius of the direct call that stops, and of the first target of the continuation")
guide_number("k_far", m2_rk$params$k, "%.0f", "hip stiffness of the second target of the continuation")
guide_number("step_r", path$step_length[4], "%.4f", "step length at r_f = 0.3 and k = 0, in leg lengths")
guide_number("period_r", path$period[4], "%.4f", "step duration at r_f = 0.3 and k = 0, in time units")
guide_number("period_rk", path$period[8], "%.4f", "step duration at r_f = 0.3 and k = 2, in time units")
guide_number("step_rk", path$step_length[8], "%.4f", "step length at r_f = 0.3 and k = 2, in leg lengths")
guide_number("n_path", nrow(path), "%d", "configurations of the continuation path, the default one included")

## ---- hidden: figure-limit-cycles ----
# one cycle of the limit cycle of a model, from the member flow() applied to the fixed point
cycle_of <- function(m) {
  f <- m$fixed_point()
  ell <- seq(0, f$period, length.out = 201)  # local time within the cycle
  y <- m$flow(f$x_star, 0, ell)
  colnames(y) <- m$reduced_state_names
  cbind(t = ell, y)
}
y_r <- cycle_of(m2_r)
col <- guide_colors()
guide_figure("limit_cycles", width_cm = 15.5, height_cm = 7.4, draw = function() {
  series <- list(list(m = m2, col = col[["blue"]]),
                 list(m = m2_r, col = col[["vermilion"]]),
                 list(m = m2_rk, col = col[["green"]]))
  labels <- c(expression(italic(r)[f] == 0 * "," ~ italic(k) == 0),
              expression(italic(r)[f] == 0.3 * "," ~ italic(k) == 0),
              expression(italic(r)[f] == 0.3 * "," ~ italic(k) == 2))
  t_max <- 8
  panels <- list(list(v = "theta", ylim = c(-0.3, 0.42), at = c(-0.2, 0, 0.2),
                      tag = expression("(a) Angle of the stance leg, " * theta * " (rad)")),
                 list(v = "phi", ylim = c(-0.55, 0.55), at = c(-0.4, 0, 0.4),
                      tag = expression("(b) Angle between the legs, " * phi * " (rad)")))
  par(mfrow = c(2, 1), oma = c(3.4, 0, 0.2, 0), mar = c(0.9, 4.2, 1.7, 0.6),
      mgp = c(2.6, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  for (i in seq_along(panels)) {
    p <- panels[[i]]
    plot(NA, xlim = c(0, t_max), ylim = p$ylim, axes = FALSE, xlab = "", ylab = "", xaxs = "i")
    abline(h = 0, col = col[["gray"]], lwd = 0.5)
    for (s in series) {
      y <- cycle_of(s$m)
      period <- s$m$fixed_point()$period
      for (j in 0:floor(t_max / period)) {
        tt <- y[, "t"] + j * period
        keep <- tt <= t_max
        lines(tt[keep], y[keep, p$v], col = s$col, lwd = 1.4)
      }
    }
    axis(2, at = p$at, col = col[["gray"]], col.axis = "black")
    if (i == length(panels)) axis(1, at = 0:t_max, col = col[["gray"]], col.axis = "black")
    title(main = p$tag, adj = 0, font.main = 1, cex.main = 1, line = 0.5)
    if (i == 1) {
      legend("topright", legend = labels, col = vapply(series, function(s) s$col, ""),
             lwd = 1.4, horiz = TRUE, bty = "n", cex = 0.95, inset = c(0, -0.03),
             x.intersp = 0.6, text.width = c(1.25, 1.45, 1.45))
    }
  }
  mtext(expression("Time (units of " * sqrt(italic(L) / italic(g)) * ")"), side = 1,
        outer = TRUE, line = 2.2)
})
if (abs(y_r[201, "theta"] + y_r[1, "theta"]) > 1e-8 || abs(y_r[201, "phi"] + y_r[1, "phi"]) > 1e-8) {
  stop("on level ground, theta and phi were expected to change sign over the step")
}
amplitude <- function(m, v) max(abs(cycle_of(m)[, v]))
if (amplitude(m2_r, "theta") <= amplitude(m2, "theta") || amplitude(m2_r, "phi") <= amplitude(m2, "phi")) {
  stop("the arced feet were expected to increase the amplitude of both angles")
}
period_ratio <- m2_rk$fixed_point()$period / m2_r$fixed_point()$period
if (period_ratio < 0.45 || period_ratio > 0.55) {
  stop("the step duration at k = 2 was expected to be about half that at k = 0")
}

## ---- floor-run ----
run <- simulate_run(m2_r, u, n_cycles = 40)
c(fell = run$fell, n_cycles_done = run$n_cycles_done)
ev <- run$events
post <- paste0(m2_r$reduced_state_names, "_post")
head(round(data.frame(cycle = ev$cycle, duration = ev$duration,
                      u_1e3 = 1000 * ev$u, ev[, post]), 4), 4)
round(range(ev$duration) - m2_r$fixed_point()$period, 3)

## ---- hidden: numbers-floor-run ----
if (run$fell) stop("the run with a floor-height input was expected to complete")
if (!identical(ev$u, u$values)) stop("the applied values were expected to be those of the input")
if (max(abs(unlist(ev[1, post]) - m2_r$fixed_point()$x_star)) > 0) {
  stop("the first row of the events was expected to hold the fixed point")
}
t_r <- m2_r$fixed_point()$period
guide_number("dur_low", abs(min(ev$duration) - t_r), "%.3f", "largest shortening of a step in the run at r_f = 0.3, in time units")
guide_number("dur_high", max(ev$duration) - t_r, "%.3f", "largest lengthening of a step in the run at r_f = 0.3, in time units")
if (ev$u[1] >= 0 || ev$duration[1] <= t_r) stop("a negative first input value was expected to lengthen the first step")
if (cor(ev$u, ev$duration) >= 0) stop("the step duration was expected to fall when the floor height rises")
guide_number("n_event_columns", ncol(ev), "%d", "columns of the table of events of M2")

## ---- impact-maps ----
x_r <- m2_r$fixed_point()$x_star
m2_p <- model_m2(r_f = 0.3, x_init = x_r, impact = "printed")
max(abs(m2_p$fixed_point()$x_star - x_r))
h <- -2e-4                                    # floor lower by 0.0002 leg length
after <- rbind(derived = m2_r$cycle_map(x_r, h)$x_next,
               printed = m2_p$cycle_map(x_r, h)$x_next)
colnames(after) <- m2_r$reduced_state_names
round(1e6 * (after["printed", ] - after["derived", ]), 2)

## ---- hidden: numbers-impact ----
# the two maps on level ground at r_f = 0.3, and after the same floor step at r_f = 0
level <- m2_p$cycle_map(x_r, 0)$x_next - m2_r$cycle_map(x_r, 0)$x_next
m0_p <- model_m2(impact = "printed")
point <- m0_p$cycle_map(fp$x_star, h)$x_next - m2$cycle_map(fp$x_star, h)$x_next
gap_impact <- after["printed", ] - after["derived", ]
if (max(abs(gap_impact[c("theta", "phi")])) > 0) {
  stop("the two impact maps were expected to give the same angles")
}
if (max(abs(level)) > 1e-12 || max(abs(point)) > 1e-12) {
  stop("the two impact maps were expected to be equal on level ground and at r_f = 0")
}
if (abs(gap_impact[["theta_dot"]]) < 1e-6) stop("the two impact maps were expected to differ after a floor step")
if (!identical(m2_p$fixed_point()$x_star, x_r)) {
  stop("the search was expected to return a known fixed point unchanged")
}
if (max(abs(m2_p$fixed_point()$eig - m2_r$fixed_point()$eig)) > 1e-6) {
  stop("the two impact maps were expected to give the same eigenvalues")
}
guide_number("h_e4", abs(1e4 * h), "%.0f", "floor step of the comparison of the impact maps, in 1e-4 leg length (lower floor)")
guide_number("impact_gap_e6", abs(1e6 * gap_impact[["theta_dot"]]), "%.1f", "difference of theta_dot after the floor step between the two impact maps, in 1e-6")
guide_number("impact_rel_e4", abs(1e4 * gap_impact[["theta_dot"]] / after["derived", "theta_dot"]), "%.1f", "relative difference of theta_dot after the floor step between the two impact maps, in 1e-4")
guide_number("impact_equal_exp", bound_exp(max(abs(level), abs(point), 1e-15)), "%d", "the two impact maps differ by less than ten to minus this number on level ground and at r_f = 0")

## ---- hidden: numbers-fall ----
# a single step up of 0.005 leg length at r_f = 0.3: the model completes the cycle that ends
# on the higher floor and falls in the next cycle
step_up <- simaci_input(c(0.005, rep(0, 9)), source = "floor_step",
                        redrawable = FALSE, value_unit = "leg length")
run_up <- simulate_run(m2_r, step_up, n_cycles = 10)
if (!run_up$fell || run_up$fall_cycle != 2L || run_up$n_cycles_done != 1L) {
  stop("the step up was expected to cause a fall in cycle 2")
}
if (nrow(run_up$events) != 1L || max(run_up$samples[, "cycle"]) != 2) {
  stop("the run was expected to keep one row of events and the samples of two cycles")
}
# in cycle 2 the stance leg is leg 2, whose angle is thG + phG in the sampled coordinates
in_fall <- run_up$samples[, "cycle"] == 2
theta_fall <- run_up$samples[in_fall, "thG"] + run_up$samples[in_fall, "phG"]
if (min(theta_fall) <= 0 || theta_fall[length(theta_fall)] < 1.1) {
  stop("the stance leg was expected to stay behind the normal and to come back near 1.2 rad")
}
guide_number("h_up_mm", 1000 * step_up$values[1], "%.0f", "step up of the fall example for a leg of 1 m, in mm")
guide_number("h_up", step_up$values[1], "%.3f", "step up of the fall example, in leg lengths")
guide_number("fall_cycle", run_up$fall_cycle, "%d", "cycle of the fall after the step up")
guide_number("fall_dur", abs(run_up$events$duration[1] - t_r), "%.3f", "shortening of the step that ends on the step up, in time units")

## ---- errors ----
writeLines(message_of(model_m2(r_f = 1)))
writeLines(message_of(model_m2(impact = "exact")))
writeLines(message_of(m2$cycle_map(fp$x_star, 0.05)))
run_x <- simulate_run(m2, u, n_cycles = 5, injection = "terrain")
run_x$injection
identical(run_x$events$duration, run_m2$events$duration[1:5])

## ---- hidden: checks-equations ----
# The equations typeset in the chapter, transcribed here from its LaTeX source, against the
# code of the package (m2_rhs, m2_impact, m2_impact_printed) on random states.
d_of <- function(th, r) (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(th)
eq_thdd <- function(y, p) {
  r <- p$r_f
  ((1 - r) * sin(y[1] - p$gamma) + r * (1 - r) * y[2]^2 * sin(y[1]) - r * sin(p$gamma)) /
    d_of(y[1], r)
}
eq_phdd <- function(y, p) {
  r <- p$r_f
  psi <- y[1] - y[3]
  (1 - r * cos(psi) - (1 - r) * cos(y[3])) * eq_thdd(y, p) + sin(psi - p$gamma) +
    (1 - r) * y[2]^2 * sin(y[3]) - p$k * y[3]
}
eq_impact <- function(y, r) {
  thm <- y[1]; thdm <- y[2]; phm <- y[3]
  thp <- thm - phm
  n <- (1 - r)^2 * cos(phm) + r * (1 - r) * (cos(thm) + cos(thm - phm)) + r^2
  thdp <- n / d_of(thp, r) * thdm
  phdp <- (1 - (1 - r) * cos(phm) - r * cos(thm)) * thdp - r * (1 - cos(thm)) * thdm
  c(thp, thdp, -phm, phdp)
}
# phi_dd as printed by Hobbelen and Wisse (2007, p. 1223), read by this agent in the PDF of
# the article; sign3 is the sign of its third term (+1 as printed)
printed_phdd <- function(y, p, sign3) {
  r <- p$r_f
  th <- y[1]; thd <- y[2]; ph <- y[3]
  dd <- d_of(th, r)
  common <- (1 - r) * sin(ph) - r * sin(th - ph)
  (1 - r) * ((1 - r) + r * cos(th)) * common * thd^2 / dd + r * (1 - r) * sin(th) * thd^2 / dd +
    sign3 * ((1 - r) * cos(th - p$gamma) + r * cos(p$gamma)) * common / dd +
    ((1 - r) * sin(th - p$gamma) - r * sin(p$gamma)) / dd - p$k * ph
}
# vector from the contact point of a foot to the hip, for a leg at angle a from the normal
to_hip <- function(a, r) c((1 - r) * sin(a), r + (1 - r) * cos(a))
set.seed(90005101)
gap <- matrix(NA_real_, 200, 8)
for (i in 1:200) {
  y <- c(runif(1, -0.4, 0.4), runif(1, -0.6, 0.6), runif(1, -0.8, 0.8), runif(1, -1, 1))
  p <- list(gamma = runif(1, 0, 0.05), k = runif(1, 0, 10), r_f = runif(1, 0, 0.9))
  acc <- walkbench:::m2_rhs(0, y, p)[[1]]
  yd <- walkbench:::m2_impact(y, p$r_f)
  yp <- walkbench:::m2_impact_printed(y, p$r_f)
  n <- (1 - p$r_f)^2 * cos(y[3]) + p$r_f * (1 - p$r_f) * (cos(y[1]) + cos(y[1] - y[3])) + p$r_f^2
  # simplest walker (Garcia et al. 1998, Eqs. 1 and 2), for the same state and slope
  p0 <- list(gamma = p$gamma, k = 0, r_f = 0)
  garcia <- sin(y[1] - p$gamma) + y[2]^2 * sin(y[3]) - cos(y[1] - p$gamma) * sin(y[3])
  gap[i, ] <- c(abs(acc[2] - eq_thdd(y, p)), abs(acc[4] - eq_phdd(y, p)),
                max(abs(yd - eq_impact(y, p$r_f))),
                abs(acc[4] - printed_phdd(y, p, -1)), abs(acc[4] - printed_phdd(y, p, +1)),
                abs(yp[2] / yd[2] - d_of(y[1] - y[3], p$r_f) / d_of(y[1], p$r_f)),
                abs(sum(to_hip(y[1], p$r_f) * to_hip(y[1] - y[3], p$r_f)) - n) +
                  abs(sum(to_hip(y[1] - y[3], p$r_f)^2) - d_of(y[1] - y[3], p$r_f)),
                abs(walkbench:::m2_rhs(0, y, p0)[[1]][4] - garcia))
}
worst <- apply(gap, 2, max)
if (any(worst[c(1, 2, 3, 4, 6, 7, 8)] > 1e-12)) {
  stop("an equation of the chapter differs from the code of the package")
}
if (worst[5] < 0.1) stop("the printed sign of the third term was expected to change phi_dd")
guide_number("eq_exp", bound_exp(max(worst[c(1, 2, 3, 4)])), "%d", "the equations of the chapter and the printed phi_dd with the reversed sign differ from the code by less than ten to minus this number, over 200 random states")
guide_number("printed_sign_gap", worst[5], "%.1f", "largest difference of phi_dd between the code and the printed equation with its printed sign, over 200 random states")
guide_number("n_states", nrow(gap), "%d", "random states of the checks of the equations")

## ---- hidden: checks-search ----
# the search of the fixed point does not test the stability of the cycle: at slope 0.02, beyond
# the range of stable period-one gaits of the simplest walker, it returns an unstable cycle
m_steep <- model_m2(gamma = 0.02)
console <- utils::capture.output(fp_steep <- m_steep$fixed_point())
if (max(Mod(fp_steep$eig)) <= 1) stop("the cycle at slope 0.02 was expected to be unstable")
# during this search, the integrator prints a message on the console
if (!any(grepl("DLSODAR", console, fixed = TRUE)) ||
    !any(grepl("too near to the initial point", console, fixed = TRUE))) {
  stop("the search at slope 0.02 was expected to print the message of the integrator")
}
guide_number("gamma_steep", m_steep$params$gamma, "%.2f", "slope of the example of an unstable cycle, in rad")
guide_number("modulus_steep", max(Mod(fp_steep$eig)), "%.2f", "largest eigenvalue modulus of the cycle found at slope 0.02")
msg_steep <- message_of(transient_cycles(m_steep))
if (!grepl("^the loop is not stable", msg_steep)) stop("transient_cycles() was expected to stop on the unstable cycle")
# a model built with a known fixed point as its guess returns it unchanged
m_again <- model_m2(r_f = 0.3, k = 2, x_init = m2_rk$fixed_point()$x_star)
if (!identical(m_again$fixed_point()$x_star, m2_rk$fixed_point()$x_star)) {
  stop("a model built with a known fixed point was expected to return it")
}
# a value of t_min above the step duration (1.97 time units at r_f = 0.3 and k = 2) makes the
# model miss its foot strike
m_late <- model_m2(r_f = 0.3, k = 2, t_min = 2.5, x_init = m2_rk$fixed_point()$x_star)
console <- utils::capture.output(msg_late <- message_of(m_late$fixed_point()))
if (msg_late != "fixed point of M2 not found") {
  stop("the search with t_min above the step duration was expected to stop with an error")
}
guide_number("t_min_late", m_late$params$t_min, "%.1f", "value of t_min above the step duration at r_f = 0.3 and k = 2, in time units")
