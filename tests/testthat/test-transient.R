# Title: Tests of the rule for the length of the transient (walkbench 0.3.0)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-06
# Description: transient_cycles() against the rule that the author chose on 2026-11-06 at
#   13:55 (decision register of the SIM-ACI project; analysis plan, draft 0.5, sections 4.3
#   and 4.8): the smallest n with lambda^(2n) <= 0.005, at least 20 cycles. M1 keeps 20
#   cycles, so that the replay of the feasibility study is unchanged; M2 in the configuration
#   of section 4.10 takes 34 cycles.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

# minimal object with the eigenvalues of a cycle map
fake_model <- function(eig) list(fixed_point = function() list(eig = eig))

test_that("transient_cycles() gives the smallest n with lambda^(2n) <= tol", {
  # 0.25^3 = 0.0156 and 0.25^4 = 0.0039
  expect_identical(transient_cycles(fake_model(c(0.5, -0.2)), minimum = 0L), 4L)
  expect_identical(transient_cycles(fake_model(0.5)), 20L)
  # complex pair: the modulus counts
  expect_identical(transient_cycles(fake_model(complex(real = 0, imaginary = 0.5)),
                                    minimum = 0L), 4L)
  for (lam in c(0.3, 0.6, 0.9, 0.95, 0.99)) {
    n <- transient_cycles(fake_model(lam), minimum = 0L)
    expect_lte(lam^(2 * n), 0.005)
    expect_gt(lam^(2 * (n - 1L)), 0.005)
  }
  # lambda = 0: lambda^0 = 1 after zero cycles, 0 after one cycle
  expect_identical(transient_cycles(fake_model(0), minimum = 0L), 1L)
  expect_error(transient_cycles(fake_model(1.01)), "not stable")
})

test_that("M1 keeps 20 transient cycles, the default of run_trial()", {
  m1 <- model_m1()
  expect_identical(transient_cycles(m1), 20L)
  # the rule alone gives 6 cycles (lambda = 0.589); the minimum applies
  expect_identical(transient_cycles(m1, minimum = 0L), 6L)
  default <- eval(formals(run_trial)$n_transient, list(model = m1))
  expect_identical(default, 20L)
})

test_that("M2 in the configuration of section 4.10 takes 34 transient cycles", {
  m2 <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
                 x_init = c(0.171841406, -0.274656028, 0.343682812, -0.008800993))
  lam <- max(Mod(m2$fixed_point()$eig))
  expect_equal(lam, 0.9229, tolerance = 1e-4)
  expect_identical(transient_cycles(m2), 34L)
  # lambda^66 = 0.00503 is above the tolerance, lambda^68 = 0.00428 below
  expect_gt(lam^66, 0.005)
  expect_lt(lam^68, 0.005)
})

# Tests added for walkbench 0.4.0 by the SP1.1 session (Claude Opus 5.5), 2026-11-06: rule of
# the closed loop, analysis plan frozen at commit ba72afe, section 4.3

test_that("transient_cycles_loop() uses the closed loop at the gain of the cell (M1)", {
  lin1 <- linearize(model_m1())
  # kappa = 0: the loop of H3b keeps the eigenvalue 1 of the uncorrected asynchrony, and the
  # transient is that of the exogenous injection
  expect_equal(closed_loop_radius(lin1, "h3b", "static", 0), 1)
  expect_identical(transient_cycles_loop(lin1, "h3b", "static", 0), 20L)
  # example of section 4.3: kappa = 0.05 in M1 gives 52 cycles
  expect_identical(transient_cycles_loop(lin1, "h3b", "static", 0.05), 52L)
  expect_identical(transient_cycles_loop(lin1, "h3b", "static", 0.10), 26L)
  expect_identical(transient_cycles_loop(lin1, "h3b", "static", 0.50), 36L)
  # H3a at g = 0 is the open loop
  expect_identical(transient_cycles_loop(lin1, "h3a", "deadbeat", 0), transient_cycles(model_m1()))
  # an unstable gain has no transient (the static loop of M1 is stable up to 0.59)
  expect_error(transient_cycles_loop(lin1, "h3b", "static", 0.70), "not stable")
  expect_error(transient_cycles_loop(lin1, "h3b", "static", -0.1), ">= 0")
  expect_identical(transient_from_radius(0.95), 52L)
})

test_that("M2: 34 to 171 transient cycles over the eight levels of g of H3a", {
  m2 <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
                 x_init = c(0.171841406, -0.274656028, 0.343682812, -0.008800993))
  lin2 <- linearize(m2)
  n <- vapply(seq(0, 0.10, length.out = 8),
              function(g) transient_cycles_loop(lin2, "h3a", "deadbeat", g), integer(1))
  expect_identical(n, c(34L, 39L, 45L, 54L, 66L, 84L, 113L, 171L))
})
