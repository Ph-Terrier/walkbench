# Title: Criterion (b): linearization of M1 against the values of SPINAL
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: specification, section 13.2. Reference values: SPINAL,
#   validation-aci/lots2_4_resultats.md, section 2.1 (repository "time series R"), with a
#   tolerance of half a unit of the last printed digit. The full comparison with
#   07a_walker_linearization.rds is a project check of SIMACI
#   (wp1_replication/02_compare_spinal_linearization.R).
# Dependencies: testthat, walkbench
# Input: none
# Output: none

test_that("the linearization of M1 reproduces the values of SPINAL", {
  lin <- linearize(model_m1())
  near <- function(x, ref, tol) all(abs(x - ref) <= tol)
  expect_true(near(lin$x_star, c(0.200311, -0.199832), 5e-7))
  expect_true(near(lin$period, 3.8825, 5e-5))
  expect_true(near(Mod(lin$eig), c(0.5891, 0.5891), 5e-5))
  expect_true(near(lin$b_vec, c(0.4996, -0.3559), 5e-5))
  expect_true(near(lin$a_vec, c(82.82, 79.80), 5e-3))
  expect_true(near(lin$e_sc, 2.302, 5e-4))
  expect_true(near(lin$static_gain, 8.125, 5e-4))
  expect_identical(max(abs(lin$psi)), 0)  # the default input acts on the strike time only
})

test_that("the nonlinear response to a single input matches the linear impulse response", {
  m1 <- model_m1()
  lin <- linearize(m1)
  one <- function(h) {
    u <- numeric(12)
    u[3] <- h
    simulate_run(m1, simaci_input(u, "impulse"), n_cycles = 12)$events$duration
  }
  resp <- (one(1e-5) - one(-1e-5)) / 2e-5
  expect_true(all(abs(resp[3:7] - lin$impulse_T[1:5]) <= 0.01 * abs(lin$impulse_T[1:5])))
})
