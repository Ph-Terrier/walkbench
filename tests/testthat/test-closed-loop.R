# Title: Tests of the closed-loop port, the metronome and the gain scales
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: specification, section 13.3, test-closed-loop.R. The stable ranges are those
#   of section 7 of the specification, computed on 2026-11-05 from the SPINAL linearization.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

test_that("a gain of zero gives the open-loop run with the same series", {
  m1 <- model_m1()
  lin <- linearize(m1)
  y0 <- m1$fixed_point()$y0 + c(1e-9, 0, 0, 0)
  w <- input_arfima(30, 0.2, 0.003, seed = 3)
  open <- simulate_run(m1, w, n_cycles = 30, y0 = y0)
  for (sc in c("deadbeat", "shift")) {
    cl <- simulate_run(m1, n_cycles = 30, y0 = y0,
                       controller = make_controller_h3a(w, 0, lin, scale = sc))
    expect_identical(cl$events$u, open$events$u)
    expect_identical(unname(cl$samples), unname(open$samples))
  }
  met <- make_metronome(lin$period)
  open_m <- simulate_run(m1, w, n_cycles = 30, y0 = y0, metronome = met)
  for (sc in c("static", "immediate", "full_state")) {
    cl <- simulate_run(m1, n_cycles = 30, y0 = y0, metronome = met,
                       controller = make_controller_h3b(w, 0, lin, scale = sc))
    expect_identical(cl$events$u, open_m$events$u)
    expect_identical(cl$events$asynchrony, open_m$events$asynchrony)
  }
})

test_that("the input of cycle k ignores the exogenous values after cycle k", {
  m1 <- model_m1()
  lin <- linearize(m1)
  w1 <- input_arfima(30, 0, 0.003, seed = 8)
  w2 <- w1
  w2$values[11:30] <- -w2$values[11:30]
  r1 <- simulate_run(m1, n_cycles = 30, controller = make_controller_h3a(w1, 0.5, lin))
  r2 <- simulate_run(m1, n_cycles = 30, controller = make_controller_h3a(w2, 0.5, lin))
  expect_identical(r1$events$u[1:10], r2$events$u[1:10])
  expect_false(identical(r1$events$u[11:30], r2$events$u[11:30]))
})

test_that("the asynchronies equal the contact times minus the beats", {
  m1 <- model_m1()
  lin <- linearize(m1)
  met <- make_metronome(lin$period)
  cs <- input_arfima(40, 0.45, 0.003, seed = 21)
  run <- simulate_run(m1, n_cycles = 40, metronome = met,
                      controller = make_controller_h3b(cs, 0.2, lin))
  expect_false(run$fell)
  expect_identical(run$events$asynchrony, run$events$t_end - run$events$cycle * lin$period)
  expect_error(simulate_run(m1, n_cycles = 5, controller = make_controller_h3b(cs, 0.2, lin)),
               "metronome")
})

test_that("the gain scales and stable ranges of section 7 are reproduced", {
  lin <- linearize(model_m1())
  expect_equal(gain_scale(lin, "h3a", "shift"), 2.002, tolerance = 5e-4)
  expect_equal(gain_scale(lin, "h3b", "immediate"), 0.4344, tolerance = 5e-4)
  expect_equal(gain_scale(lin, "h3b", "static"), 0.1231, tolerance = 5e-4)
  expect_equal(stable_gain_limit(lin, "h3a", "shift"), 0.52)
  expect_equal(stable_gain_limit(lin, "h3a", "deadbeat"), 1.09)
  expect_equal(stable_gain_limit(lin, "h3b", "immediate"), 0.16)
  expect_equal(stable_gain_limit(lin, "h3b", "static"), 0.59)
  expect_equal(stable_gain_limit(lin, "h3b", "full_state"), 0.24)
  # slowest eigenvalue of the static loop equals 1 - kappa at small gains
  m <- closed_loop_matrix(lin, "h3b", "static", 0.05)
  expect_equal(max(Re(eigen(m)$values)), 0.950, tolerance = 5e-4)
})
