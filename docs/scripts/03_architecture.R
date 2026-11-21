# Title: Example and figure of the chapter "Architecture and conventions" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: builds one object of each kind that the parts of walkbench 0.7.0 exchange
#   (model, input, run, metronome, settings of the detrended fluctuation analysis, estimation
#   settings, linearization), prints their classes, and checks the conventions that the
#   chapter states (numbering of the cycles, units, classes). Draws the diagram of the flow of
#   a run and of a trial. Seed: 90,003,001 (cell 30 of the block of the guide).
# Dependencies: walkbench 0.7.0 (loaded by the driver), graphics, grDevices
# Input: none
# Output: generated/03_architecture/: snippets/*.txt, output/*.txt, numbers.tex, numbers.csv,
#   fig_flow.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/03_architecture.R

## ---- hidden: setup ----
library(graphics)
library(grDevices)
# whether deSolve is attached before the first use of model M1 (checked below)
desolve_attached_before <- "package:deSolve" %in% search()

## ---- objects ----
m1 <- model_m1()
u <- input_iid_uniform(25, rho = 0.004, seed = 90003001)
run <- simulate_run(m1, u, n_cycles = 25)
kept <- trim_run(run, n_transient = 20)
objects <- list(model = m1, input = u, run = run, metronome = make_metronome(3.9),
                dfa = dfa_settings(), settings = m1$estimation_settings,
                linearization = linearize(m1), row = estimate_all(kept, m1))
sapply(objects, function(x) class(x)[1])

## ---- conventions ----
m1$time_unit
c(cycle = m1$cycle_unit, cycles_per_stride = m1$cycles_per_stride)
run$events[1:3, c("cycle", "t_start", "t_end", "duration", "leg")]
kept$events[1:2, c("cycle", "t_start", "t_end", "duration", "leg")]
c(simulated = run$n_cycles_done, kept = kept$n_cycles_done)

## ---- hidden: checks ----
cls <- sapply(objects, function(x) class(x)[1])
expected <- c(model = "simaci_model", input = "simaci_input", run = "simaci_run",
              metronome = "simaci_metronome", dfa = "simaci_dfa", settings = "list",
              linearization = "list", row = "data.frame")
if (!identical(cls, expected)) stop("the classes of the objects changed")
ev <- run$events
# cycle k ends at the contact that starts cycle k + 1, and the stance leg alternates
if (max(abs(ev$t_end[-nrow(ev)] - ev$t_start[-1])) > 0) stop("the cycles are not contiguous")
if (!identical(ev$leg[1:4], c(1L, 2L, 1L, 2L))) stop("the stance leg does not alternate from 1")
if (ev$t_start[1] != 0) stop("the first cycle does not start at time 0")
if (kept$events$t_start[1] != 0 || kept$events$cycle[1] != 1) stop("trim_run() does not rebase")
if (!identical(model_m2()$cycle_unit, "step")) stop("the cycle of M2 is not the step")
if (!identical(model_m2()$time_unit, m1$time_unit)) stop("the two models have different time units")
# the pinned file of M1 calls library(deSolve): the first use of M1 attaches that package
if (desolve_attached_before || !("package:deSolve" %in% search())) {
  stop("deSolve was expected to be attached by the first use of model M1, and only then")
}
guide_number("n_cycles", run$n_cycles_done, "%d", "cycles of the run of the example")
guide_number("n_kept", kept$n_cycles_done, "%d", "kept cycles of the example")
# post-generation fix by Claude Fable 5.2, 2026-11-14: the chapter reads the number of removed
# cycles from the run (fix 03-W1 of review/pass2_status.md)
guide_number("n_transient", run$n_cycles_done - kept$n_cycles_done, "%d",
             "transient cycles removed in the example")
guide_number("n_members", length(m1), "%d", "members of a model object")

## ---- hidden: figure-flow ----
col <- guide_colors()
guide_figure("flow", width_cm = 15.5, height_cm = 9.4, draw = function() {
  par(mar = c(0, 0, 0, 0))
  plot(NA, xlim = c(0, 100), ylim = c(-3, 56), axes = FALSE, xlab = "", ylab = "")
  box_at <- function(x, y, w, h, label, fill = "white") {
    rect(x - w / 2, y - h / 2, x + w / 2, y + h / 2, col = fill, border = col[["gray"]], lwd = 0.8)
    text(x, y, label, cex = 0.86)
  }
  arrow <- function(x0, y0, x1, y1) {
    arrows(x0, y0, x1, y1, length = 0.06, angle = 25, lwd = 0.9, col = col[["gray"]])
  }
  fn <- "#EAF3FA"   # light blue: functions of the package; white: objects
  # frame of a trial
  rect(3.5, 14, 99, 55, border = col[["gray"]], lty = 2, lwd = 0.8)
  text(6, 52.2, "run_trial(): one attempt per seed, until a run completes", adj = 0, cex = 0.86)
  # objects that enter a run
  box_at(17, 44, 22, 7, "model")
  box_at(17, 33, 22, 7, "input and initial state,
drawn from the seed")
  box_at(17, 22, 22, 7, "controller, metronome
(closed loop)")
  # simulation and outcomes
  box_at(45, 33, 20, 7, "simulate_run()", fill = fn)
  arrow(28, 44, 35, 35.8)
  arrow(28, 33, 35, 33)
  arrow(28, 22, 35, 30.2)
  box_at(66, 33, 12, 7, "run")
  arrow(55, 33, 60, 33)
  box_at(87, 33, 18, 7, "trim_run()", fill = fn)
  arrow(72, 33, 78, 33)
  box_at(87, 21, 18, 7, "estimate_all()", fill = fn)
  arrow(87, 29.5, 87, 24.5)
  box_at(66, 21, 18, 7, "summary row")
  arrow(78, 21, 75, 21)
  text(66, 42, "fall or numerical failure:
next attempt", cex = 0.78, col = col[["gray"]])
  # linearization and its two uses, outside the trial
  box_at(17, 4.5, 22, 7, "linearize()", fill = fn)
  segments(6, 44, 1.5, 44, lwd = 0.9, col = col[["gray"]])
  segments(1.5, 44, 1.5, 4.5, lwd = 0.9, col = col[["gray"]])
  arrow(1.5, 4.5, 6, 4.5)
  box_at(52, 9, 34, 6, "gain scales, stability of the closed loop")
  box_at(52, 0.5, 34, 6, "analytic predictor")
  arrow(28, 5.5, 35, 9)
  arrow(28, 3.5, 35, 0.5)
})
