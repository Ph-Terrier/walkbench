# Title: Worked example of the chapter "Runs and trials" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the writer agent of Chapter 7 (Claude Fable 5.2), an agent of the
#   guide session of SIM-ACI dispatched by that session (Claude Fable 5.2), without a
#   code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-13
# Description: simulates runs of model M1 with the run driver of walkbench 0.6.2 and shows the
#   run object (samples, events, bounds), the removal of the transient cycles, the rule of the
#   transient, a trial with its attempts, the seed rule, the check of the master seeds and the
#   count of a numerical failure. Every input holds independent uniform values of one
#   amplitude (rho = 0.026 rad), chosen so that M1 falls in some attempts. The example is a
#   demonstration computed for the guide. Its seeds lie in the block 90,000,000 to 92,099,999:
#   the inputs built directly use 90,007,001 and 90,007,002, the fixed draw uses 90,007,301,
#   and the trials use the seed rule with the master seed 90,000,000 and the cells 71 and 73.
#   The numerical failure is provoked: a copy of the model calls the solver of deSolve once
#   with tolerances that the solver rejects, as the tests of the package do. The blocks whose
#   label starts with "hidden:" write the signatures of the functions, register the numbers
#   quoted in the chapter, check the statements and the error messages of the text and draw
#   the figure; the other blocks are printed in the chapter with their output. One hidden
#   check runs three unperturbed cycles of model M2 at its default parameters.
# Dependencies: walkbench 0.6.2 (loaded by the driver), deSolve, stats, graphics, grDevices,
#   utils
# Input: none
# Output: generated/07_runs_and_trials/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_contact.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/07_runs_and_trials.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
library(utils)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
for (f in c("simulate_run", "trim_run", "transient_cycles", "transient_from_radius",
            "run_trial", "draw_single_seed", "draw_fixed", "seed_campaign",
            "check_master_seeds")) guide_signature(f)

## ---- run ----
m1 <- model_m1()
n_transient <- transient_cycles(m1)
n_keep <- 200L
n_total <- n_transient + n_keep
rho <- 0.026
u <- input_iid_uniform(n_total, rho = rho, seed = 90007002)
run <- simulate_run(m1, u, n_cycles = n_total)
dim(run$samples)
head(run$samples, 2)
range(table(run$samples[, "cycle"]))              # samples per cycle
run$events[1:2, c("cycle", "t_start", "t_end", "duration", "u", "leg")]

## ---- hidden: numbers-run ----
smp <- run$samples
ev <- run$events
guide_number("n_transient", n_transient, "%d", "transient cycles of M1, transient_cycles(model_m1())")
guide_number("n_keep", n_keep, "%d", "kept cycles of the example")
guide_number("n_total", n_total, "%d", "cycles simulated in each run of the example")
guide_number("rho", 1000 * rho, "%.0f", "amplitude rho of the uniform inputs, in 1e-3 rad")
guide_number("rho_bound", 1000 * rho / 2, "%.0f", "bound rho / 2 of the uniform inputs, in 1e-3 rad")
guide_number("dt", run$dt, "%.2f", "sampling step of M1, in model time units")
guide_number("n_fields", length(run), "%d", "fields of a run object")
guide_number("n_samples", nrow(smp), "%d", "rows of the matrix samples of the run")
guide_number("n_event_cols", ncol(ev), "%d", "columns of the table events of M1")
guide_number("samples_min", min(table(smp[, "cycle"])), "%d", "smallest number of samples in a cycle of the run")
guide_number("samples_max", max(table(smp[, "cycle"])), "%d", "largest number of samples in a cycle of the run")
guide_number("t_end_run", ev$t_end[n_total], "%.2f", "time of the last contact of the run, in model time units")
guide_number("t_first_sample", smp[1, "t"], "%.2f", "time of the first sample of the run")
guide_number("t_last_sample", smp[nrow(smp), "t"], "%.2f", "time of the last sample of the run")
guide_number("duration_min", min(ev$duration), "%.2f", "shortest step duration of the run, in model time units")
guide_number("duration_max", max(ev$duration), "%.2f", "longest step duration of the run, in model time units")
if (run$fell || run$n_cycles_done != n_total) stop("the first run was expected to complete")
if (!inherits(run, "simaci_run")) stop("the run was expected to have the class simaci_run")
if (!identical(names(run), c("model", "injection", "samples", "events", "bounds", "fell",
                             "fall_cycle", "n_cycles_done", "dt", "input", "metronome_period",
                             "n_redraws", "seeds", "n_transient", "meta"))) {
  stop("the fields of the run object differ from the table of the chapter")
}
if (!identical(names(run$meta), c("interface_version", "start", "wall_time_s"))) {
  stop("the field meta differs from the table of the chapter")
}
if (!identical(names(ev), c("cycle", "t_start", "t_end", "duration", "u", "u_exo", "u_corr",
                            "deviation", "asynchrony", "beat", "leg", "theta_post",
                            "theta_dot_post"))) {
  stop("the columns of the table events differ from the table of the chapter")
}
if (!identical(colnames(smp), c("t", "thG", "thGd", "phG", "phGd", "cycle"))) stop("the columns of samples differ from the text")
if (!identical(run$input, u)) stop("the run was expected to store the input unchanged")
if (!identical(m1$fixed_point()$y0, as.numeric(run$bounds[[1]]$start) * c(1, 1, -1, -1))) {
  stop("the first exact state was expected to be the default initial state")
}
# every sample lies on the global grid, and every grid point up to the last contact is present
if (max(abs(smp[, "t"] / run$dt - round(smp[, "t"] / run$dt))) > 1e-6) stop("a sample lies off the grid")
if (nrow(smp) != floor(ev$t_end[n_total] / run$dt)) stop("the number of samples differs from the number of grid points")
if (!isTRUE(all.equal(diff(smp[, "t"]), rep(run$dt, nrow(smp) - 1L)))) stop("the grid has a gap")
# the samples of cycle k lie after its start and not after its end
lo <- ev$t_start[smp[, "cycle"]]
hi <- ev$t_end[smp[, "cycle"]]
if (any(smp[, "t"] <= lo | smp[, "t"] > hi)) stop("a sample lies outside its cycle")
# open loop without metronome: the columns of the controller and of the metronome are empty
if (!all(is.na(ev[, c("u_exo", "u_corr", "deviation", "asynchrony", "beat")]))) stop("columns expected empty")
if (!identical(ev$u, u$values)) stop("the column u was expected to hold the input")
if (!identical(ev$leg, rep(1:2, n_total / 2))) stop("the stance leg was expected to alternate from 1")
if (!isTRUE(all.equal(ev$t_end - ev$t_start, ev$duration))) stop("duration differs from t_end - t_start")
if (!identical(ev$t_start[-1], ev$t_end[-n_total])) stop("a cycle was expected to start at the end of the previous one")
if (length(run$bounds) != n_total) stop("one element of bounds per cycle was expected")

## ---- bounds ----
k <- 21
run$events[k + 0:1, c("cycle", "t_start", "t_end", "u", "leg")]
round(rbind(end_21 = run$bounds[[k]]$end[1, ],
            start_22 = run$bounds[[k + 1]]$start[1, ]), 5)

## ---- hidden: numbers-bounds ----
t_c <- ev$t_end[k]
end_k <- run$bounds[[k]]$end[1, ]
start_k1 <- run$bounds[[k + 1]]$start[1, ]
guide_number("k_contact", k, "%d", "cycle whose end is shown in the figure of the contact")
guide_number("k_next", k + 1L, "%d", "cycle that starts at the contact of the figure")
guide_number("t_contact", t_c, "%.5f", "time of the contact that ends cycle 21, in model time units")
guide_number("t_before", max(smp[smp[, "t"] < t_c, "t"]), "%.2f", "last grid time before the contact")
guide_number("t_after", min(smp[smp[, "t"] > t_c, "t"]), "%.2f", "first grid time after the contact")
guide_number("thgd_end", end_k[["thGd"]], "%.4f", "angular velocity of leg 1 at the end of cycle 21")
guide_number("thgd_start", start_k1[["thGd"]], "%.4f", "angular velocity of leg 1 at the start of cycle 22")
guide_number("thg_contact", end_k[["thG"]], "%.4f", "angle of leg 1 at the contact that ends cycle 21")
if (abs(end_k[["thG"]] - start_k1[["thG"]]) > 1e-12) stop("the angle of leg 1 was expected to be continuous at the contact")
if (abs(end_k[["thGd"]] - start_k1[["thGd"]]) < 1e-3) stop("the angular velocity of leg 1 was expected to jump at the contact")
if (smp[smp[, "t"] > t_c, "cycle"][1] != k + 1L) stop("the first sample after the contact was expected in cycle 22")
# at the default injection point, the relative angle of the legs changes at the contact by the
# input of the cycle that ends at the contact (value after the contact minus value before it)
if (abs(start_k1[["phG"]] - end_k[["phG"]] - ev$u[k]) > 1e-9) stop("the change of the relative angle at the contact was expected to equal the input of cycle 21")

## ---- hidden: figure-contact ----
col <- guide_colors()
guide_figure("contact", width_cm = 15.5, height_cm = 6.4, draw = function() {
  half <- 0.3
  sel <- which(abs(smp[, "t"] - t_c) < half)
  s <- smp[sel, , drop = FALSE]
  before <- s[, "cycle"] == k
  panels <- list(list(v = "thG", tag = "(a) Angle of leg 1 (rad)"),
                 list(v = "thGd", tag = "(b) Angular velocity of leg 1 (rad per time unit)"))
  par(mfrow = c(2, 1), oma = c(3.2, 0, 0.2, 0), mar = c(0.9, 4.6, 1.7, 0.6),
      mgp = c(2.6, 0.6, 0), tcl = -0.3, las = 1)
  par(cex = 1)
  for (i in seq_along(panels)) {
    v <- panels[[i]]$v
    y_all <- c(s[, v], end_k[[v]], start_k1[[v]])
    plot(s[, "t"], s[, v], type = "n", axes = FALSE, xlab = "", ylab = "",
         xlim = t_c + c(-half, half), ylim = extendrange(y_all, f = 0.12))
    abline(v = t_c, col = col[["gray"]], lty = 3)
    lines(c(s[before, "t"], t_c), c(s[before, v], end_k[[v]]), col = col[["gray"]], lwd = 0.6)
    lines(c(t_c, s[!before, "t"]), c(start_k1[[v]], s[!before, v]), col = col[["gray"]], lwd = 0.6)
    points(s[, "t"], s[, v], pch = 16, col = col[["blue"]], cex = 0.9)
    points(t_c, end_k[[v]], pch = 1, col = col[["vermilion"]], cex = 1.8, lwd = 1.4)
    points(t_c, start_k1[[v]], pch = 4, col = col[["green"]], cex = 1.1, lwd = 1.4)
    axis(1, at = s[, "t"], labels = FALSE, tcl = -0.2, col = col[["gray"]])
    if (i == length(panels)) {
      axis(1, at = seq(80.2, 80.7, by = 0.1), col = col[["gray"]], col.axis = "black")
    }
    axis(2, at = pretty(y_all, n = 3), col = col[["gray"]], col.axis = "black")
    title(main = panels[[i]]$tag, adj = 0, font.main = 1, cex.main = 1, line = 0.5)
    if (i == 1) {
      # legend in the top margin, at the right of the panel tag
      legend("topright", inset = c(0, -0.5), xpd = NA, horiz = TRUE, bty = "n", cex = 0.9,
             pt.cex = c(0.9, 1.5, 1.1), pt.lwd = c(1, 1.4, 1.4), pch = c(16, 1, 4),
             col = col[c("blue", "vermilion", "green")], x.intersp = 0.9, text.width = NA,
             legend = c("grid sample", "end of cycle 21", "start of cycle 22"))
    }
  }
  mtext("Time (model time units)", side = 1, outer = TRUE, line = 1.9)
})

## ---- trim ----
kept <- trim_run(run, n_transient)
c(n_cycles_done = kept$n_cycles_done, n_transient = kept$n_transient)
kept$events[1:2, c("cycle", "t_start", "t_end", "duration", "u")]
u_fall <- input_iid_uniform(n_total, rho = rho, seed = 90007001)
run_fall <- simulate_run(m1, u_fall, n_cycles = n_total)
c(fall_cycle = run_fall$fall_cycle, n_cycles_done = run_fall$n_cycles_done)
tryCatch(trim_run(run_fall, n_transient), error = conditionMessage)

## ---- hidden: numbers-trim ----
guide_number("t_start_kept", ev$t_start[n_transient + 1L], "%.5f", "start of the first kept cycle in the untrimmed run, in model time units")
guide_number("first_kept", n_transient + 1L, "%d", "number of the first kept cycle in the untrimmed run")
guide_number("fall_cycle", run_fall$fall_cycle, "%d", "cycle of the fall of the second run")
guide_number("fall_done", run_fall$n_cycles_done, "%d", "completed cycles of the second run")
if (!identical(kept$events$cycle, seq_len(n_keep))) stop("the kept cycles were expected to be numbered from 1")
if (!identical(range(kept$samples[, "cycle"]), c(1, n_keep))) stop("the kept samples were expected to carry the cycles 1 to n_keep")
if (kept$events$t_start[1] != 0) stop("the first kept cycle was expected to start at time 0")
if (!identical(rownames(kept$events)[1], as.character(n_transient + 1L))) stop("the row names were expected to keep the original numbers")
if (!identical(kept$events$duration, ev$duration[n_transient + seq_len(n_keep)])) stop("the kept step durations differ")
if (!identical(kept$events$u, u$values[n_transient + seq_len(n_keep)])) stop("the kept column u differs from the kept part of the input")
if (!identical(kept$bounds, run$bounds[n_transient + seq_len(n_keep)])) stop("the kept bounds differ")
if (!identical(kept$input, u)) stop("the input was expected to stay whole")
kept_rows <- smp[, "cycle"] > n_transient
if (!isTRUE(all.equal(unname(kept$samples[, "t"]), unname(smp[kept_rows, "t"] - ev$t_start[n_transient + 1L])))) {
  stop("the times of the kept samples were expected to be rebased")
}
if (!identical(unname(kept$samples[, 2:5]), unname(smp[kept_rows, 2:5]))) stop("the kept states differ")
if (!run_fall$fell) stop("the second run was expected to fall")
if (nrow(run_fall$events) != run_fall$n_cycles_done || length(run_fall$bounds) != run_fall$n_cycles_done) {
  stop("events and bounds were expected to hold the completed cycles")
}
if (max(run_fall$samples[, "cycle"]) != run_fall$fall_cycle) stop("the samples of the cycle of the fall were expected in the run")
if (!identical(m1$fall_rule, "abs(theta) >= 1.2 rad, or no heel strike within 15 time units")) stop("the fall rule of M1 differs from the text")

## ---- transient ----
lambda <- max(Mod(m1$fixed_point()$eig))
round(lambda, 4)
c(rule = transient_cycles(m1, minimum = 0L), default = transient_cycles(m1))
mu <- c(0.3, 0.8, 0.9, 0.95, 0.99)
n_rule <- vapply(mu, transient_from_radius, integer(1), minimum = 0L)
n_default <- vapply(mu, transient_from_radius, integer(1))
data.frame(mu, n_rule, n_default, mu_2n = signif(mu^(2 * n_rule), 3))

## ---- hidden: numbers-transient ----
guide_number("lambda_m1", lambda, "%.4f", "largest modulus of the eigenvalues of the cycle map of M1 at the fixed point")
guide_number("n_rule_m1", transient_cycles(m1, minimum = 0L), "%d", "transient cycles of M1 under the rule without the minimum")
guide_number("lambda_m1_2n", lambda^(2 * transient_cycles(m1, minimum = 0L)), "%.5f", "lambda^(2n) of M1 at n = 6")
guide_number("lambda_m1_2n_less", lambda^(2 * (transient_cycles(m1, minimum = 0L) - 1L)), "%.5f", "lambda^(2n) of M1 at n = 5")
guide_number("n_less_m1", transient_cycles(m1, minimum = 0L) - 1L, "%d", "number of cycles one below the rule for M1")
guide_number("n_rule_099", transient_from_radius(0.99), "%d", "transient cycles for a modulus of 0.99")
if (any(mu^(2 * n_rule) > 0.005) || any(mu^(2 * (n_rule - 1L)) <= 0.005)) stop("n_rule is not the smallest n with mu^(2n) <= 0.005")
if (!identical(n_default[mu <= 0.8], rep(20L, 2)) || any(n_default[mu > 0.8] <= 20L)) stop("the minimum was expected to apply up to a modulus of 0.8")
if (transient_from_radius(0, minimum = 0L) != 1L) stop("a modulus of 0 was expected to give one cycle")

## ---- trial ----
make_input <- function(n, seed) input_iid_uniform(n, rho = rho, seed = seed)
draw <- draw_single_seed(m1, make_input, n = n_total)
seed_of <- function(attempt) {
  seed_campaign(90000000, cell = 71, replicate = 7, attempt = attempt)
}
trial <- run_trial(m1, draw, seed_of, n_keep = n_keep, cell = 71,
                   replicate = 7, keep_run = TRUE)
trial$seeds
seed_campaign(90000000, cell = 71, replicate = 7, attempt = 0:2)
names(trial$summary)
trial$summary[, c("model", "injection", "cell", "replicate")]
trial$summary[, c("n_redraws", "fell", "n_falls", "n_numerical_failures")]

## ---- hidden: numbers-trial ----
guide_number("trial_cell", 71, "%d", "cell of the first trial")
guide_number("trial_replicate", 7, "%d", "replicate of the first trial")
guide_number("trial_attempts", length(trial$seeds), "%d", "attempts of the first trial")
guide_number("n_summary_cols", ncol(trial$summary), "%d", "columns of the summary row of M1")
guide_number("n_outcome_cols", ncol(trial$summary) - 8L, "%d", "outcome columns of the summary row of M1")
fall_cycle_of <- function(seed) {
  d <- draw(seed)
  simulate_run(m1, d$input, n_cycles = n_total, y0 = d$y0)$fall_cycle
}
guide_number("trial_fall0", fall_cycle_of(trial$seeds[1]), "%d", "cycle of the fall in attempt 0 of the first trial")
guide_number("trial_fall1", fall_cycle_of(trial$seeds[2]), "%d", "cycle of the fall in attempt 1 of the first trial")
if (trial$fell || length(trial$seeds) != 3L || trial$n_falls != 2L) stop("the first trial was expected to complete at attempt 2 after two falls")
if (!identical(names(trial), c("summary", "divergence", "seeds", "n_redraws", "fell", "n_falls",
                               "n_numerical_failures", "numerical_failures",
                               "integrator_warnings", "wall_time_s", "run"))) {
  stop("the fields of the result of a trial differ from the table of the chapter")
}
if (!identical(names(trial$summary)[c(1:6, 28:29)],
               c("model", "injection", "cell", "replicate", "n_redraws", "fell", "n_falls",
                 "n_numerical_failures"))) stop("the bookkeeping columns differ from the text")
if (trial$n_redraws != 2L || trial$n_numerical_failures != 0L) stop("the counts of the first trial differ from the text")
if (length(trial$numerical_failures) != 0L || length(trial$integrator_warnings) != 0L) stop("no message of the solver was expected")
if (!identical(trial$run$seeds, trial$seeds) || trial$run$n_redraws != 2L) stop("the kept run was expected to record the seeds and the new draws")
if (trial$run$n_cycles_done != n_keep || trial$run$n_transient != n_transient) stop("the kept run was expected to be without its transient cycles")
if (run$n_transient != 0L) stop("the field n_transient of an untrimmed run was expected to be 0")
if (!is.list(trial$divergence) || !is.null(attr(trial$summary, "divergence"))) stop("the divergence curve was expected in the field divergence only")

## ---- hidden: checks-no-outcomes ----
# the same trial, limited to one attempt: a trial without outcomes
trial_0 <- run_trial(m1, draw, seed_of, n_keep = n_keep, cell = 71,
                     replicate = 7, max_redraws = 0L, keep_run = TRUE)
if (!trial_0$fell || length(trial_0$seeds) != 1L || trial_0$n_falls != 1L) stop("the trial limited to one attempt was expected to end without outcomes")
if (!identical(names(trial_0), names(trial)[1:10])) stop("the fields of a trial without outcomes differ from the text")
if (!is.null(trial_0$divergence) || !is.null(trial_0$run)) stop("a trial without outcomes was expected to have no divergence curve and no run")
if (!identical(names(trial_0$summary), names(trial$summary))) stop("the summary row of a trial without outcomes was expected to have the columns of a completed trial")
if (!all(is.na(trial_0$summary[, 7:27]))) stop("the outcome columns of a trial without outcomes were expected to be empty")

## ---- hidden: checks-draws ----
# the draws of attempt 2 of the trial, repeated outside the driver: after set.seed(), the
# values of the input come first, then the four values of the offset of the initial state
d <- draw(trial$seeds[3])
set.seed(trial$seeds[3])
values <- runif(n_total, -0.5, 0.5) * rho
offset <- rnorm(4) * 1e-9
if (!identical(d$input$values, values)) stop("the input of attempt 2 differs from the first draws after set.seed()")
if (!identical(d$y0, m1$fixed_point()$y0 + offset)) stop("the initial state of attempt 2 differs from the fixed point plus the next four draws")
if (max(abs(offset)) > 1e-8 || min(abs(offset)) < 1e-11) stop("the offset was expected to be of the order of 1e-9")

## ---- hidden: checks-seeds ----
# check of two sets of master seeds, quoted in the function reference: the seven master seeds
# of the campaigns pass; 90,000,000 and 91,000,000 fail, and attempt 10 of the first one has
# the seed of attempt 0 of the second one
if (!isTRUE(check_master_seeds(campaign_table()$master))) stop("the master seeds of the campaigns were expected to pass the check")
if (!identical(check_master_seeds(c(90000000, 91000000)), FALSE)) stop("the master seeds 90,000,000 and 91,000,000 were expected to fail the check")
if (!identical(seed_campaign(c(90000000, 91000000), cell = 0, replicate = 0, attempt = c(10, 0)),
               c(91000000L, 91000000L))) stop("the two master seeds were expected to share the seed 91,000,000")
if (!identical(as.numeric(seed_campaign(90000000, 71, 7, 0:2)), trial$seeds)) stop("the seeds of the trial differ from the seed rule")
if (!identical(trial$run$events$u, values[n_transient + seq_len(n_keep)])) stop("the run of the trial was expected to hold the kept part of the drawn values")
if (!is.na(d$input$seed)) stop("the field seed of a drawn input was expected to be NA")
if (!is.integer(seed_campaign(90000000, 71, 7, 0)) || !is.double(trial$seeds)) stop("types of the seeds differ from the text")
if (seed_campaign(90000000, 999, 99, 20) - 90000000L != 2099999L) stop("the block of a master seed was expected to hold 2,100,000 seeds")
if (seed_campaign(max(campaign_table()$master), 999, 99, 20) != 172099999L) stop("the largest seed of the rule differs from the text")
if (!identical(diff(campaign_table()$master), rep(10000000L, 6))) stop("the master seeds of the campaigns were expected to differ by 10,000,000")
if (!isTRUE(check_master_seeds(90000000)) || !isTRUE(check_master_seeds(c(90000000, 92100000))) ||
    isTRUE(check_master_seeds(c(90000000, 92099999)))) stop("check_master_seeds() differs from the text")
# draw_fixed(): the input is the object that the function received, and the offset is the
# first draw after set.seed()
fixed <- draw_fixed(m1, u)
a <- fixed(90007301)
set.seed(90007301)
if (!identical(a$input, u) || !identical(a$y0, m1$fixed_point()$y0 + rnorm(4) * 1e-9)) stop("draw_fixed() differs from the text")

## ---- failure ----
oscillator <- function(t, y, parms) list(c(y[2], -y[1]))
m1_test <- m1
n_calls <- 0
m1_test$simulate <- function(...) {
  n_calls <<- n_calls + 1
  if (n_calls == 1) {                             # attempt 0 only
    capture.output(deSolve::lsodar(c(1, 0), c(0, 1), oscillator, NULL,
                                   rtol = 1e-30, atol = 1e-30))
  }
  m1$simulate(...)
}
trial_2 <- run_trial(m1_test, draw, seed_of, n_keep = n_keep, cell = 71,
                     replicate = 7)
c(attempts = length(trial_2$seeds), n_falls = trial_2$n_falls,
  n_numerical_failures = trial_2$n_numerical_failures)
writeLines(strwrap(trial_2$numerical_failures, width = 60))

## ---- hidden: numbers-failure ----
guide_number("desolve_version", as.character(utils::packageVersion("deSolve")), "%s", "version of deSolve that ran the example")
if (trial_2$fell || trial_2$n_numerical_failures != 1L || trial_2$n_falls != 1L || length(trial_2$seeds) != 3L) {
  stop("the trial with a provoked failure was expected to count one failure and one fall, then to complete")
}
if (!identical(trial_2$seeds, trial$seeds)) stop("the trial with a provoked failure was expected to use the seeds of the first trial")
# attempt 2 is the completed attempt of the first trial: same outcome columns
if (!identical(trial_2$summary[, 7:27], trial$summary[, 7:27])) stop("the outcomes were expected to equal those of the first trial")
if (n_calls != 3) stop("three calls of the member simulate were expected")
if (length(trial_2$integrator_warnings) != 0L) stop("no warning of the solver was expected")
# the two kinds of conditions of the solver that the text names, with this version of deSolve:
# an error for a rejected input, a warning for an error return during the integration
kind <- NULL
invisible(capture.output(kind <- tryCatch({
  deSolve::lsodar(c(1, 0), c(0, 1e5), oscillator, NULL, maxsteps = 10)
  "none"
}, warning = function(w) "warning", error = function(e) "error")))
if (!identical(kind, "warning")) stop("the solver was expected to report a return on maxsteps as a warning")

## ---- hidden: checks-errors ----
# messages of the table of errors of the chapter, compared verbatim
expect_error_message <- function(expr, text) {
  got <- tryCatch({ expr; "no error" }, error = conditionMessage)
  if (!identical(got, text)) stop("message expected: ", text, "; message obtained: ", got)
}
expect_error_message(simulate_run(list(), u, n_cycles = 10), "model must be a simaci_model")
expect_error_message(simulate_run(m1, n_cycles = 10), "an open-loop run needs an input")
expect_error_message(simulate_run(m1, u, n_cycles = n_total + 1L),
                     "input too short: 220 values for 221 cycles")
expect_error_message(simulate_run(m1, u, n_cycles = 10, injection = "floor_height"),
                     "unknown injection point of M1: floor_height")
expect_error_message(trim_run(run_fall, n_transient), "a run that fell is not trimmed")
expect_error_message(transient_from_radius(1),
                     "the loop is not stable (largest eigenvalue modulus 1.000000)")
expect_error_message(transient_from_radius(Inf),
                     "the loop is not stable (largest eigenvalue modulus Inf)")
expect_error_message(seed_campaign(90000000, cell = 1000, replicate = 1), "cell must lie in 0 to 999")
expect_error_message(seed_campaign(90000000, cell = 1, replicate = 100), "replicate must lie in 0 to 99")
expect_error_message(seed_campaign(90000000, cell = 71, replicate = 7, attempt = 21),
                     "attempt must lie in 0 to 20")
expect_error_message(seed_campaign(125000000, cell = 1, replicate = 1),
                     "a master seed of 100,000,000 or more must be that of a campaign (campaign_master())")
expect_error_message(seed_campaign(120000000, cell = 0, replicate = 1),
                     "in a campaign, cells and replicates are numbered from 1")
expect_error_message(seed_campaign(0, cell = 1, replicate = 1), "seed outside 1 to 2^31 - 1")
# an error that the solver did not raise stops the trial with its own message
draw_short <- draw_single_seed(m1, make_input, n = n_total - 1L)
expect_error_message(run_trial(m1, draw_short, seed_of, n_keep = n_keep),
                     "input too short: 219 values for 220 cycles")
# conditions that pass without an error, named in the chapter
odd <- trim_run(run, n_total)
if (nrow(odd$events) != 2L || !is.na(odd$events$cycle[1])) stop("trim_run() with n_transient = n_cycles_done differs from the text")
twice <- trim_run(trim_run(run, 10L), 10L)
if (twice$n_cycles_done != n_total - 20L || twice$n_transient != 10L) stop("trim_run() applied twice differs from the text")
# a controller given with an input: the input is checked and stored, the controller sets u
zero <- function(k, info) list(u = 0)
both <- simulate_run(m1, u, n_cycles = 4, controller = zero)
if (!all(both$events$u == 0) || !identical(both$input, u)) stop("a run with a controller and an input differs from the text")
alone <- simulate_run(m1, n_cycles = 4, controller = zero)
if (!is.null(alone$input) || alone$fell) stop("a run with a controller and no input differs from the text")
# a metronome in open loop fills the columns beat and asynchrony; trim_run() keeps beat
beats <- simulate_run(m1, u, n_cycles = 8, metronome = make_metronome(m1$fixed_point()$period))
if (anyNA(beats$events$beat) || !isTRUE(all.equal(beats$events$asynchrony, beats$events$t_end - beats$events$beat))) {
  stop("the columns of the metronome differ from the text")
}
if (!identical(trim_run(beats, 3L)$events$beat, beats$events$beat[4:8])) stop("trim_run() was expected to keep the column beat")
if (!identical(beats$metronome_period, m1$fixed_point()$period)) stop("metronome_period differs from the text")
# draw_fixed() does not read the field redrawable: with a redrawable input, the driver makes
# further attempts with the same input (seeds of cell 73)
same_input <- run_trial(m1, draw_fixed(m1, u_fall), function(attempt) {
  seed_campaign(90000000, cell = 73, replicate = 1, attempt = attempt)
}, n_keep = n_keep, max_redraws = 2L)
if (length(same_input$seeds) != 3L || same_input$n_falls != 3L) stop("draw_fixed() with a redrawable input differs from the text")
# model M2 at its default parameters: 15 columns in events, and no check of the injection point
m2 <- model_m2()
run_m2 <- simulate_run(m2, simaci_input(numeric(3), "zero", value_unit = "leg length"),
                       n_cycles = 3, injection = "not_a_point")
if (run_m2$fell || !identical(run_m2$injection, "not_a_point")) stop("model M2 was expected to accept any name of injection point")
if (!identical(names(run_m2$events)[12:15], c("theta_post", "theta_dot_post", "phi_post", "phi_dot_post"))) {
  stop("the columns of the table events of M2 differ from the text")
}
if (!identical(colnames(run_m2$samples), colnames(run$samples))) stop("the columns of samples of M2 differ from those of M1")
if (m2$params$dt != m1$params$dt) stop("the default sampling steps of M1 and M2 were expected to be equal")
