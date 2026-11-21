# Title: Tests of the rule for numerical failures (walkbench 0.6.0 and 0.6.1)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model; revised by the same session on 2026-11-07 for walkbench 0.6.1, on
#   the task of the coordination session (Claude Opus 5.5) after the answer of the author of
#   2026-11-07 at 07:31 (decision register of the SIM-ACI project); revised again on 2026-11-07
#   for walkbench 0.6.2 (finding F1 of the review of the code of campaigns 2 and 3 of M1,
#   option B of the author of 08:17, triage approved at 08:41, register)
# Date: 2026-11-06 (revised 2026-11-07)
# Description: section 4 of update 1 of the registration osf.io/m74tk. An error of the
#   integrator is a numerical failure, distinct from a fall; with a generated input the next
#   attempt draws a new input and the failures count among the 21 attempts; with a fixed input
#   the trial ends as after a fall; summarize_cells() reports the failures apart, the combined
#   rate, and flags a cell when the fall rate or the combined rate exceeds 20%. Since walkbench
#   0.6.1, only an error raised by the compiled solver of deSolve is a numerical failure: the
#   fake models call deSolve::lsodar() with tolerances below the machine precision, which the
#   solver rejects before the first step (istate = -3). Any other error stops run_trial() with
#   its original message: an error of the model code, of the right-hand side, an argument check
#   of the R code of deSolve, and lsodar() not found while deSolve is detached, the defect of the
#   parallel workers found on 2026-11-07. Since walkbench 0.6.2, an attempt that ends as a fall
#   after a warning of the solver (an error return that deSolve reports as a warning) is a
#   numerical failure; a warning of the model code leaves a fall a fall, and an attempt that
#   completes after a warning of the solver keeps its outcomes. Fake models keep the tests
#   fast.
# Dependencies: testthat, walkbench, deSolve
# Input: none
# Output: none

rhs_oscillator <- function(t, y, p) list(c(y[2], -y[1]))
# error of the compiled solver of deSolve: tolerances below the machine precision make lsodar()
# reject its input before the first step (istate = -3); the solver prints its message, which
# capture.output() keeps out of the test log
solver_error <- function() {
  invisible(utils::capture.output(
    deSolve::lsodar(c(1, 0), c(0, 1), rhs_oscillator, NULL, rtol = 1e-30, atol = 1e-30)))
}
# fake model: the first value of the input decides the outcome of an attempt (1: error of the
# integrator, 2: fall)
fake_model <- function() {
  structure(list(id = "fake", label = "fake", cycle_unit = "step", default_injection = "none",
                 params = list(dt = 0.01), fixed_point = function() list(y0 = c(0, 0)),
                 simulate = function(input, n_cycles, y0, controller, metronome, injection) {
                   if (input$values[1] == 1) solver_error()
                   list(fell = TRUE, fall_cycle = 2L, n_cycles_done = 2L)
                 }),
            class = "simaci_model")
}
# the same fake model, whose simulation runs `f` instead
fake_model_with <- function(f) {
  m <- fake_model()
  m$simulate <- function(input, n_cycles, y0, controller, metronome, injection) f()
  m
}
# generated input: odd attempts fail numerically, even attempts fall (seed = attempt)
draw_alternating <- function(seed) {
  list(input = simaci_input(rep(if (seed %% 2 == 1) 1 else 2, 60), "fake"), y0 = c(0, 0))
}
# fixed input: one attempt only
draw_fixed_fake <- function(seed) {
  list(input = input_fixed_series(rep(c(1, 2), 30), 0L, 1), y0 = c(0, 0))
}
trial_error <- function(m) {
  tryCatch(run_trial(m, draw_fixed_fake, seed_of = function(a) a + 1, n_transient = 0L,
                     n_keep = 50L),
           error = function(e) e)
}

test_that("numerical failures are counted apart and redrawn within the 21 attempts", {
  tr <- run_trial(fake_model(), draw_alternating, seed_of = function(a) a, n_transient = 0L,
                  n_keep = 50L)
  expect_true(tr$fell)
  expect_identical(length(tr$seeds), 21L)
  expect_identical(tr$n_falls, 11L)
  expect_identical(tr$n_numerical_failures, 10L)
  expect_identical(length(tr$numerical_failures), 10L)
  expect_match(tr$numerical_failures[1], "illegal input", fixed = TRUE)
  expect_identical(tr$summary$n_falls, 11L)
  expect_identical(tr$summary$n_numerical_failures, 10L)
  expect_identical(tr$integrator_warnings, character(0))
})

test_that("a numerical failure with a fixed input ends the trial as a fall", {
  tr <- run_trial(fake_model_with(solver_error), draw_fixed_fake, seed_of = function(a) a + 1,
                  n_transient = 0L, n_keep = 50L)
  expect_true(tr$fell)
  expect_identical(length(tr$seeds), 1L)
  expect_identical(tr$n_numerical_failures, 1L)
  expect_identical(tr$n_falls, 0L)
})

test_that("an error of the model code stops the trial with its original message and call", {
  e <- trial_error(fake_model_with(function() stop("programming error (fake)")))
  expect_s3_class(e, "error")
  expect_identical(conditionMessage(e), "programming error (fake)")
  expect_false(is.null(conditionCall(e)))
})

test_that("a function not found stops the trial, although its call names lsodar()", {
  # lsodar() looked up where no function of that name exists: the call of the error is
  # lsodar(...), the call of the integrator, but lsodar() never ran
  e <- trial_error(fake_model_with(function() {
    eval(quote(lsodar(c(1, 0), c(0, 1), rhs_oscillator, NULL)), new.env(parent = emptyenv()))
  }))
  expect_s3_class(e, "error")
  expect_identical(as.character(conditionCall(e)[[1]]), "lsodar")
  expect_match(conditionMessage(e), "lsodar", fixed = TRUE)
})

test_that("M1 with deSolve detached stops the trial: the defect of the parallel workers", {
  m1 <- model_m1()
  invisible(m1$fixed_point())
  expect_true("package:deSolve" %in% search())  # attached by the pinned walker file
  draw <- draw_fixed(m1, input_fixed_series(rep(c(-1, 1), 30), 20L, 1e-3, value_unit = "rad"))
  run_detached <- function() {
    detach("package:deSolve", character.only = TRUE)
    # the attach can warn that deSolve was built under another version of R
    on.exit(suppressWarnings(suppressPackageStartupMessages(library(deSolve))), add = TRUE)
    tryCatch(run_trial(m1, draw, seed_of = function(a) a + 1, n_transient = 20L,
                       n_keep = 40L),
             error = function(e) e)
  }
  e <- run_detached()
  expect_true("package:deSolve" %in% search())
  expect_s3_class(e, "error")
  expect_identical(as.character(conditionCall(e)[[1]]), "lsodar")
  expect_match(conditionMessage(e), "lsodar", fixed = TRUE)
})

test_that("errors of the right-hand side and argument checks of deSolve stop the trial", {
  rhs_fails <- function(t, y, p) {
    if (t > 0.5) stop("right-hand side failed (fake)")
    list(c(y[2], -y[1]))
  }
  e1 <- trial_error(fake_model_with(function() {
    deSolve::lsodar(c(1, 0), seq(0, 1, 0.1), rhs_fails, NULL)
  }))
  expect_identical(conditionMessage(e1), "right-hand side failed (fake)")
  # stop() of the R code of deSolve, which checks the length of the derivatives
  e2 <- trial_error(fake_model_with(function() {
    deSolve::lsodar(c(1, 0), c(0, 1), function(t, y, p) list(y[2]), NULL)
  }))
  expect_s3_class(e2, "error")
  expect_match(conditionMessage(e2), "number of derivatives", fixed = TRUE)
})

# error return of the solver (maxsteps exceeded), which deSolve reports as two warnings with a
# truncated output; the fake model then records a fall, as the loops of M1 and M2 do
solver_warning_then_fall <- function() {
  invisible(utils::capture.output(
    deSolve::lsodar(c(1, 0), c(0, 1e5), rhs_oscillator, NULL, maxsteps = 10)))
  list(fell = TRUE, fall_cycle = 2L, n_cycles_done = 2L)
}

test_that("a fall after a warning of the solver is a numerical failure", {
  tr <- suppressWarnings(run_trial(fake_model_with(solver_warning_then_fall), draw_fixed_fake,
                                   seed_of = function(a) a + 1, n_transient = 0L,
                                   n_keep = 50L))
  expect_true(tr$fell)
  expect_identical(tr$n_falls, 0L)
  expect_identical(tr$n_numerical_failures, 1L)
  expect_identical(tr$summary$n_numerical_failures, 1L)
  expect_match(tr$numerical_failures[1], "^an excessive amount of work")
  expect_match(tr$numerical_failures[1], "Returning early", fixed = TRUE)
  expect_identical(length(tr$integrator_warnings), 2L)
  expect_match(tr$integrator_warnings[1], "^attempt 0: an excessive amount of work")
})

test_that("with a generated input, such failures are redrawn within the 21 attempts", {
  m <- fake_model()
  m$simulate <- function(input, n_cycles, y0, controller, metronome, injection) {
    if (input$values[1] == 1) return(solver_warning_then_fall())
    list(fell = TRUE, fall_cycle = 2L, n_cycles_done = 2L)
  }
  # odd attempts: warning of the solver, then fall; even attempts: fall
  tr <- suppressWarnings(run_trial(m, draw_alternating, seed_of = function(a) a,
                                   n_transient = 0L, n_keep = 50L))
  expect_identical(length(tr$seeds), 21L)
  expect_identical(tr$n_numerical_failures, 10L)
  expect_identical(tr$n_falls, 11L)
  expect_identical(length(tr$integrator_warnings), 20L)
})

test_that("a warning of the model code leaves a fall a fall", {
  m <- fake_model_with(function() {
    warning("warning of the model code (fake)")
    list(fell = TRUE, fall_cycle = 2L, n_cycles_done = 2L)
  })
  tr <- suppressWarnings(run_trial(m, draw_fixed_fake, seed_of = function(a) a + 1,
                                   n_transient = 0L, n_keep = 50L))
  expect_identical(tr$n_falls, 1L)
  expect_identical(tr$n_numerical_failures, 0L)
  expect_identical(tr$integrator_warnings, character(0))
})

test_that("an attempt that completes after a warning of the solver keeps its outcomes", {
  m1 <- model_m1()
  m <- m1
  m$simulate <- function(...) {
    invisible(utils::capture.output(
      deSolve::lsodar(c(1, 0), c(0, 1e5), rhs_oscillator, NULL, maxsteps = 10)))
    m1$simulate(...)
  }
  inp <- input_fixed_series(sin(seq_len(60)), 20L, 1e-3, value_unit = "rad")
  tr <- suppressWarnings(run_trial(m, draw_fixed(m1, inp), seed_of = function(a) 97000001 + a,
                                   n_transient = 20L, n_keep = 60L))
  expect_false(tr$fell)
  expect_identical(tr$n_numerical_failures, 0L)
  expect_identical(tr$n_falls, 0L)
  expect_identical(length(tr$integrator_warnings), 2L)
  expect_false(is.na(tr$summary$lambda_s))
})

test_that("summarize_cells() reports the failures, the combined rate and the flag", {
  rows <- data.frame(cell = rep(1:2, each = 5), replicate = rep(1:5, 2),
                     fell = c(rep(FALSE, 5), FALSE, FALSE, FALSE, FALSE, TRUE),
                     n_redraws = c(0, 0, 1, 0, 0, 0, 1, 0, 0, 20),
                     n_falls = c(0, 0, 0, 0, 0, 0, 1, 0, 0, 3),
                     n_numerical_failures = c(0, 0, 1, 0, 0, 0, 0, 0, 0, 18))
  s <- summarize_cells(rows, planned = 5)
  # cell 1: 6 attempts, no fall, 1 failure; cell 2: 26 attempts, 4 falls, 18 failures
  expect_equal(s$attempts, c(6, 26))
  expect_equal(s$falls, c(0, 4))
  expect_equal(s$numerical_failures, c(1, 18))
  expect_equal(s$fall_rate, c(0, 4 / 26))
  expect_equal(s$combined_rate, c(1 / 6, 22 / 26))
  expect_identical(s$flag_fall_rate, c(FALSE, FALSE))
  expect_identical(s$flag, c(FALSE, TRUE))
  expect_identical(s$included, c(TRUE, TRUE))
})
