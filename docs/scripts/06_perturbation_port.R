# Title: Worked example of the chapter "Perturbation port" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: builds inputs with the functions of the perturbation port of walkbench 0.6.2
#   (uniform independent values, Gaussian ARFIMA(0,d,0) series, a fixed series with a circular
#   start and its surrogate, an input written directly, a generator defined by the user),
#   applies them to model M1 and runs two trials through the run driver. The example is a
#   demonstration computed for the guide. Its seeds lie in the block 90,000,000 to 92,099,999,
#   which check_seed_plan() finds apart from the seed blocks of the SIM-ACI campaigns: the
#   inputs built directly use 90,000,001 to 90,000,005, and the trials use the seed rule with
#   the master seed 90,000,000 and the cells 6 and 7. The seeds of the surrogates, 70,001,001
#   to 70,001,010, are fixed by the rule of the package. The fixed series is synthetic: no
#   recorded stride series is read or copied. The blocks whose label starts with "hidden:"
#   write the signatures of the functions, register the numbers quoted in the chapter and draw
#   its two figures; the other blocks are printed in the chapter with their output.
#   Change of 2026-11-14 by the guide session (Claude Opus 5.5), on the decision of the author
#   recorded in section 12 of design_note.md (point 3): the input of the first trial is drawn
#   with d = 0 (uncorrelated Gaussian values), because run_trial() computes the divergence and
#   DFA exponents of the completed attempt, and the writer brief asks for an uncorrelated input
#   in any example that computes them. Its standard deviation goes from 0.008 to 0.007 rad: with
#   d = 0, at 0.008 rad, 15 of 16 attempts fell; a scan of 0.004 to 0.0075 rad (attempts 0 to 5)
#   gave a fall in attempt 0 and a completed attempt 1 at 0.0065, 0.007 and 0.0075 rad.
# Dependencies: walkbench 0.6.2 (loaded by the driver), stats, graphics, grDevices
# Input: none
# Output: generated/06_perturbation_port/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_inputs.pdf, fig_pulse.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/06_perturbation_port.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
for (f in c("simaci_input", "input_iid_uniform", "input_arfima", "input_fixed_series",
            "seed_surrogate", "input_surrogate", "check_input")) guide_signature(f)

## ---- model ----
m1 <- model_m1()
m1$cycle_unit
m1$default_injection
m1$injection_points$foot_strike_timing$value_unit
n_transient <- transient_cycles(m1)
n_keep <- 300L
n_total <- n_transient + n_keep
c(n_transient = n_transient, n_keep = n_keep, n_total = n_total)

## ---- generated-inputs ----
u_iid <- input_iid_uniform(n_total, rho = 0.004, seed = 90000001)
round(1000 * range(u_iid$values), 3)                 # in 1e-3 rad
u_fi <- input_arfima(n_total, d = 0.3, sd = 0.002, seed = 90000002,
                     n_transient = n_transient)
kept <- n_transient + seq_len(n_keep)
round(1000 * c(mean_all = mean(u_fi$values), sd_all = sd(u_fi$values),
               mean_kept = mean(u_fi$values[kept]),
               sd_kept = sd(u_fi$values[kept])), 4)  # in 1e-3 rad
str(u_fi[-1], digits.d = 8)                          # every field but the values

## ---- hidden: numbers-generated ----
guide_number("n_transient", n_transient, "%d", "transient cycles of M1, transient_cycles(model_m1())")
guide_number("n_keep", n_keep, "%d", "kept cycles of the example")
guide_number("n_total", n_total, "%d", "cycles simulated in the example")
guide_number("iid_rho", 1000 * u_iid$params$rho, "%.0f", "rho of the uniform input, in 1e-3 rad")
guide_number("iid_bound", 1000 * u_iid$params$rho / 2, "%.0f", "bound rho / 2 of the uniform input, in 1e-3 rad")
guide_number("fi_d", u_fi$params$d, "%.1f", "memory parameter of the ARFIMA input")
guide_number("fi_sd", 1000 * u_fi$params$sd, "%.0f", "sd of the ARFIMA input, in 1e-3 rad")
guide_number("fi_sd_all", 1000 * sd(u_fi$values), "%.4f", "sd of the ARFIMA input over all cycles, in 1e-3 rad")

## ---- fixed-series ----
stride_s <- 1.10 + input_arfima(n_keep, d = 0.4, sd = 0.025, seed = 90000003)$values
u_fix <- input_fixed_series(stride_s, n_transient = n_transient, sd = 0.002,
                            source = "synthetic_record", value_unit = "rad")
length(u_fix$values)
u_fix$redrawable
all.equal(u_fix$values[seq_len(n_transient)], tail(u_fix$values, n_transient))
u_sur <- input_surrogate(stride_s, s = 1, i = 1, n_transient = n_transient,
                         sd = 0.002, value_unit = "rad")
u_sur$seed
all.equal(sort(u_sur$values[kept]), sort(u_fix$values[kept]))
round(c(alpha_window = estimate_dfa(u_fix$values[kept]),
        alpha_surrogate = estimate_dfa(u_sur$values[kept])), 2)

## ---- hidden: numbers-fixed ----
guide_number("fix_mean_s", mean(stride_s), "%.2f", "mean of the synthetic record, in s")
guide_number("fix_sd_ms", 1000 * sd(stride_s), "%.0f", "sd of the synthetic record, in ms")
guide_number("fix_length", length(u_fix$values), "%d", "length of the fixed input")
guide_number("fix_alpha", estimate_dfa(u_fix$values[kept]), "%.2f", "DFA alpha of the window of the fixed input")
guide_number("sur_alpha", estimate_dfa(u_sur$values[kept]), "%.2f", "DFA alpha of the window of surrogate 1")
guide_number("sur_seed", u_sur$seed, "%d", "seed of surrogate 1, seed_surrogate(1, 1)")
sur_alphas <- vapply(1:10, function(i) {
  estimate_dfa(input_surrogate(stride_s, s = 1, i = i, n_transient = n_transient,
                               sd = 0.002)$values[kept])
}, numeric(1))
guide_number("sur_alpha_min", min(sur_alphas), "%.2f", "smallest DFA alpha of the windows of surrogates 1 to 10")
guide_number("sur_alpha_max", max(sur_alphas), "%.2f", "largest DFA alpha of the windows of surrogates 1 to 10")
guide_number("sur_alpha_mean", mean(sur_alphas), "%.2f", "mean DFA alpha of the windows of surrogates 1 to 10")

## ---- custom-input ----
u_pulse <- simaci_input(replace(numeric(60), 31, 0.002), source = "pulse",
                        redrawable = FALSE, value_unit = "rad")
run_pulse <- simulate_run(m1, u_pulse, n_cycles = 60)
t_star <- m1$fixed_point()$period
round(1000 * (run_pulse$events$duration - t_star)[30:36], 3)

## ---- hidden: numbers-pulse ----
dev_pulse <- 1000 * (run_pulse$events$duration - t_star)
guide_number("t_star", t_star, "%.4f", "step duration of the unperturbed limit cycle of M1, in model time units")
guide_number("pulse_u", 1000 * u_pulse$values[31], "%.0f", "value of the pulse, in 1e-3 rad")
guide_number("pulse_cycle", 31, "%d", "cycle of the pulse")
guide_number("pulse_dev_lag0", dev_pulse[31], "%.1f", "deviation of the step duration in the cycle of the pulse, in 1e-3 time units")
guide_number("pulse_dev_lag1", dev_pulse[32], "%.1f", "deviation of the step duration one cycle after the pulse, in 1e-3 time units")
guide_number("pulse_dev_lag2", dev_pulse[33], "%.1f", "deviation of the step duration two cycles after the pulse, in 1e-3 time units")
guide_number("pulse_last_cycle", max(which(abs(dev_pulse) > 0.1)), "%d", "last cycle with a deviation of the step duration above 1e-4 time units")
guide_number("pulse_n_after", max(which(abs(dev_pulse) > 0.1)) - 31, "%d", "cycles after the pulse up to the last deviation above 1e-4 time units")
if (max(abs(dev_pulse[1:30])) > 1e-4) stop("the step duration was expected to equal T* before the pulse")

## ---- custom-generator ----
input_ar1 <- function(n, phi, sd, seed = NULL, n_transient = 0L) {
  if (!is.null(seed)) set.seed(seed)
  x <- as.numeric(arima.sim(list(ar = phi), n = n))
  kept <- x[(n_transient + 1L):n]
  simaci_input((x - mean(kept)) / sd(kept) * sd, source = "ar1",
               params = list(n = n, phi = phi, sd = sd, n_transient = n_transient),
               seed = if (is.null(seed)) NA_integer_ else seed,
               redrawable = TRUE, value_unit = "rad", cycle_unit = "step")
}
u_ar <- input_ar1(n_total, phi = 0.5, sd = 0.002, seed = 90000004,
                  n_transient = n_transient)
isTRUE(check_input(u_ar, m1, n_cycles = n_total))

## ---- run ----
run <- simulate_run(m1, u_fi, n_cycles = n_total)
run$fell
run$n_cycles_done
head(run$events[, c("cycle", "t_start", "duration", "u")], 4)
identical(run$events$u, u_fi$values)
run_kept <- trim_run(run, n_transient)
identical(run_kept$events$u, u_fi$values[kept])

## ---- trial ----
draw <- draw_single_seed(m1, function(n, seed) {
  input_arfima(n, d = 0, sd = 0.007, seed = seed, n_transient = n_transient)
}, n = n_total)
seed_of <- function(attempt) {
  seed_campaign(90000000, cell = 6, replicate = 2, attempt = attempt)
}
trial <- run_trial(m1, draw, seed_of, n_transient = n_transient, n_keep = n_keep,
                   cell = 6, replicate = 2)
trial$seeds
c(n_redraws = trial$n_redraws, n_falls = trial$n_falls)
trial$summary[, c("cell", "replicate", "n_redraws", "fell", "alpha_input")]

## ---- hidden: numbers-trial ----
guide_number("trial_sd", 7, "%d", "sd of the ARFIMA input of the trial, in 1e-3 rad")
guide_number("trial_d", 0, "%d", "memory parameter d of the ARFIMA input of the trial")
guide_number("trial_seed0", trial$seeds[1], "%d", "seed of attempt 0 of the trial")
guide_number("trial_seed1", trial$seeds[2], "%d", "seed of attempt 1 of the trial")
guide_number("trial_n_redraws", trial$n_redraws, "%d", "redraws of the trial")
guide_number("trial_alpha_input", trial$summary$alpha_input, "%.2f", "DFA alpha of the kept part of the input of the completed attempt")
first <- draw(trial$seeds[1])
run_first <- simulate_run(m1, first$input, n_cycles = n_total, y0 = first$y0)
if (!run_first$fell) stop("attempt 0 of the trial was expected to fall")
guide_number("trial_fall_cycle", run_first$fall_cycle, "%d", "cycle of the fall in attempt 0 of the trial")

## ---- trial-fixed ----
u_big <- input_fixed_series(stride_s, n_transient = n_transient, sd = 0.012,
                            source = "synthetic_record", value_unit = "rad")
seed_of_fixed <- function(attempt) {
  seed_campaign(90000000, cell = 7, replicate = 1, attempt = attempt)
}
trial_fix <- run_trial(m1, draw_fixed(m1, u_big), seed_of_fixed,
                       n_transient = n_transient, n_keep = n_keep,
                       cell = 7, replicate = 1)
c(attempts = length(trial_fix$seeds), n_falls = trial_fix$n_falls)
trial_fix$fell

## ---- hidden: numbers-trial-fixed ----
guide_number("trial_fix_sd", 12, "%d", "sd of the fixed input of the second trial, in 1e-3 rad")
guide_number("trial_fix_attempts", length(trial_fix$seeds), "%d", "attempts of the trial with a fixed input")
if (!trial_fix$fell || length(trial_fix$seeds) != 1L) stop("the fixed input was expected to end the trial at its first attempt")

## ---- errors ----
message_of <- function(expr) {
  tryCatch({ expr; "no error" }, error = conditionMessage)
}
message_of(check_input(u_fi, m1, n_cycles = n_total + 1))
message_of(check_input(simaci_input(c(0, NA, 0), source = "test"), m1, 3))
message_of(check_input(simaci_input(numeric(3), source = "test",
                                    cycle_unit = "stride"), m1, 3))
message_of(input_arfima(100, d = 0.5, sd = 0.002, seed = 90000005))
message_of(input_fixed_series(c(1.1, 1.2), n_transient = 0, sd = 0.002))

## ---- hidden: figure-inputs ----
col <- guide_colors()
guide_figure("inputs", width_cm = 15.5, height_cm = 12.5, draw = function() {
  panels <- list(list(x = u_iid, tag = "(a) Uniform independent values"),
                 list(x = u_fi, tag = "(b) ARFIMA(0, d, 0) series, d = 0.3"),
                 list(x = u_fix, tag = "(c) Fixed series with a circular start"),
                 list(x = u_sur, tag = "(d) Surrogate of the fixed series"))
  par(mfrow = c(4, 1), oma = c(3.2, 0, 0.2, 0), mar = c(0.5, 4.2, 1.6, 0.6),
      mgp = c(2.6, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  for (i in seq_along(panels)) {
    v <- 1000 * panels[[i]]$x$values
    plot(seq_along(v), v, type = "n", axes = FALSE, xlab = "", ylab = "", ylim = c(-7, 7),
         xlim = c(1, n_total), xaxs = "i")
    rect(0.5, -7, n_transient + 0.5, 7, col = col[["light"]], border = NA)
    abline(h = 0, col = col[["gray"]], lwd = 0.5)
    lines(seq_along(v), v, col = col[["blue"]], lwd = 0.9)
    if (i == 3) {
      last <- (n_total - n_transient + 1L):n_total
      lines(seq_len(n_transient), v[seq_len(n_transient)], col = col[["vermilion"]], lwd = 1.3)
      lines(last, v[last], col = col[["vermilion"]], lwd = 1.3)
      text(n_total - n_transient - 3, -5.9, "last 20 values of the window", adj = 1, cex = 0.9)
      text(n_transient + 4, -5.9, "copy of these values", adj = 0, cex = 0.9)
    }
    if (i == 1) text(n_transient + 4, 5.6, "transient (20 cycles)", adj = 0, cex = 0.9)
    axis(2, at = c(-6, -3, 0, 3, 6), col = col[["gray"]], col.axis = "black")
    if (i == length(panels)) axis(1, at = c(1, 50, 100, 150, 200, 250, 300, 320),
                                  col = col[["gray"]], col.axis = "black")
    title(main = panels[[i]]$tag, adj = 0, font.main = 1, cex.main = 1, line = 0.5)
  }
  mtext("Cycle of the model (step)", side = 1, outer = TRUE, line = 1.9)
  mtext(expression("Input (" * 10^-3 * " rad)"), side = 2, outer = TRUE, line = -1.4, las = 0)
})

## ---- hidden: figure-pulse ----
guide_figure("pulse", width_cm = 15.5, height_cm = 8.5, draw = function() {
  k <- 25:45
  par(mfrow = c(2, 1), oma = c(3.2, 0, 0.2, 0), mar = c(0.9, 4.6, 1.7, 0.6),
      mgp = c(2.6, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  plot(k, 1000 * u_pulse$values[k], type = "n", axes = FALSE, xlab = "", ylab = "",
       ylim = c(-0.2, 2.4))
  abline(h = 0, col = col[["gray"]], lwd = 0.5)
  segments(k, 0, k, 1000 * u_pulse$values[k], col = col[["blue"]], lwd = 1.6)
  points(k, 1000 * u_pulse$values[k], pch = 16, col = col[["blue"]], cex = 0.9)
  axis(2, at = c(0, 1, 2), col = col[["gray"]])
  title(main = expression("(a) Input (" * 10^-3 * " rad)"), adj = 0, font.main = 1,
        cex.main = 1, line = 0.5)
  plot(k, dev_pulse[k], type = "n", axes = FALSE, xlab = "", ylab = "", ylim = c(-22, 30))
  abline(h = 0, col = col[["gray"]], lwd = 0.5)
  segments(k, 0, k, dev_pulse[k], col = col[["vermilion"]], lwd = 1.6)
  points(k, dev_pulse[k], pch = 16, col = col[["vermilion"]], cex = 0.9)
  axis(2, at = c(-20, -10, 0, 10, 20, 30), col = col[["gray"]])
  axis(1, at = seq(25, 45, by = 5), col = col[["gray"]])
  title(main = expression("(b) Deviation of the step duration from " * italic(T) * "* (" *
                            10^-3 * " time units)"),
        adj = 0, font.main = 1, cex.main = 1, line = 0.5)
  mtext("Cycle of the model (step)", side = 1, outer = TRUE, line = 1.9)
})
