# Title: Tests of the dynamics of model M2 (arced feet and hip spring)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), which carries out
#   SP1.3, without a code-generation model
# Date: 2026-11-06
# Description: the equations of M2 against (i) the equations printed by Hobbelen and Wisse
#   (2007, IEEE Trans Robot 23:1213-1224, appendix, p. 1223), transcribed below, with the sign
#   of the third term of phi_dd reversed (the derivation is in
#   SIMACI/wp1_replication/04_m2_equations_check.py); (ii) the simplest walker of the pinned
#   feasibility file at r_f = 0 and k = 0; (iii) the energy of the hip mass during a step.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

printed_phdd <- function(y, gam, k, r, sign3) {
  th <- y[1]; thd <- y[2]; ph <- y[3]
  dd <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(th)
  common <- (1 - r) * sin(ph) - r * sin(th - ph)
  (1 - r) * ((1 - r) + r * cos(th)) * common * thd^2 / dd + r * (1 - r) * sin(th) * thd^2 / dd +
    sign3 * ((1 - r) * cos(th - gam) + r * cos(gam)) * common / dd +
    ((1 - r) * sin(th - gam) - r * sin(gam)) / dd - k * ph
}
printed_thdd <- function(y, gam, r) {
  th <- y[1]; thd <- y[2]
  dd <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(th)
  ((1 - r) * sin(th - gam) + r * (1 - r) * sin(th) * thd^2 - r * sin(gam)) / dd
}

test_that("the equations of motion equal the printed ones, with the corrected sign", {
  set.seed(42)
  for (i in 1:200) {
    y <- c(runif(1, -0.4, 0.4), runif(1, -0.6, 0.6), runif(1, -0.8, 0.8), runif(1, -1, 1))
    p <- list(gamma = runif(1, 0, 0.05), k = runif(1, 0, 10), r_f = runif(1, 0, 0.9))
    acc <- walkbench:::m2_rhs(0, y, p)[[1]]
    expect_lt(abs(acc[2] - printed_thdd(y, p$gamma, p$r_f)), 1e-12)
    expect_lt(abs(acc[4] - printed_phdd(y, p$gamma, p$k, p$r_f, -1)), 1e-12)
  }
})

test_that("with r_f = 0 and k = 0 the equations are those of the simplest walker", {
  w <- walkbench:::m1_walker_env()
  set.seed(7)
  printed_gap <- numeric(50)
  for (i in 1:50) {
    y <- c(runif(1, -0.4, 0.4), runif(1, -0.6, 0.6), runif(1, -0.8, 0.8), runif(1, -1, 1))
    a2 <- walkbench:::m2_rhs(0, y, list(gamma = 0.009, k = 0, r_f = 0))[[1]]
    a1 <- w$walker_rhs(0, y, list(gamma = 0.009))[[1]]
    expect_lt(max(abs(a2 - a1)), 1e-12)
    printed_gap[i] <- abs(printed_phdd(y, 0.009, 0, 0, +1) - a1[4])
  }
  # with the printed sign of the third term, phi_dd differs from the simplest walker
  expect_gt(max(printed_gap), 0.1)
})

test_that("the impact map equals the printed one on level ground", {
  set.seed(9)
  for (i in 1:100) {
    a <- runif(1, 0.05, 0.4)
    r <- runif(1, 0, 0.9)
    thm <- -a; phm <- -2 * a; thdm <- runif(1, -0.6, -0.05)
    yp <- walkbench:::m2_impact(c(thm, thdm, phm, 0), r)
    dm <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(thm)
    thdp_pr <- ((1 - r)^2 * cos(phm) + r * (1 - r) * (cos(thm - phm) + cos(thm)) + r^2) / dm * thdm
    phdp_pr <- (1 - r) * ((-r * (1 - r) + (1 - r) * cos(phm) * (1 - (1 - r) * cos(phm))) / dm +
                            (r * (cos(thm) - cos(thm - phm)) + r * (1 - r) * sin(thm) * sin(phm)) / dm +
                            (r * (sin(thm) - sin(thm - phm)) * (r * sin(thm) + (1 - r) * sin(phm))) / dm) * thdm
    expect_lt(abs(yp[1] - (thm - phm)), 1e-15)
    expect_lt(abs(yp[3] + phm), 1e-15)
    expect_lt(abs(yp[2] - thdp_pr), 1e-12)
    expect_lt(abs(yp[4] - phdp_pr), 1e-12)
  }
})

test_that("the printed impact map equals the derived map on level ground and at r_f = 0", {
  set.seed(11)
  for (i in 1:100) {
    a <- runif(1, 0.05, 0.4)
    r <- runif(1, 0, 0.9)
    y_level <- c(-a, runif(1, -0.6, -0.05), -2 * a, runif(1, -0.2, 0.2))
    expect_lt(max(abs(walkbench:::m2_impact_printed(y_level, r) -
                        walkbench:::m2_impact(y_level, r))), 1e-12)
    y_any <- c(runif(1, -0.4, -0.05), runif(1, -0.6, -0.05), runif(1, -0.8, -0.1), runif(1, -1, 1))
    expect_lt(max(abs(walkbench:::m2_impact_printed(y_any, 0) - walkbench:::m2_impact(y_any, 0))),
              1e-12)
  }
})

test_that("the printed impact map matches the transcription of the paper off level ground", {
  set.seed(12)
  for (i in 1:100) {
    r <- runif(1, 0.1, 0.9)
    thm <- runif(1, -0.4, -0.05); phm <- 2 * thm + runif(1, -0.05, 0.05); thdm <- runif(1, -0.6, -0.05)
    yp <- walkbench:::m2_impact_printed(c(thm, thdm, phm, 0), r)
    dm <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(thm)
    thdp_pr <- ((1 - r)^2 * cos(phm) + r * (1 - r) * (cos(thm - phm) + cos(thm)) + r^2) / dm * thdm
    expect_lt(abs(yp[2] - thdp_pr), 1e-12)
    # theta_dot+ of the two maps differ by the ratio of the denominators at theta+ and theta-
    yd <- walkbench:::m2_impact(c(thm, thdm, phm, 0), r)
    dp <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(thm - phm)
    expect_lt(abs(yp[2] / yd[2] - dp / dm), 1e-12)
  }
})

test_that("the printed impact map keeps the fixed point and changes the response to a floor step", {
  x <- model_m2(gamma = 0.004)$fixed_point()$x_star
  for (rf in c(0.1, 0.2, 0.3)) x <- model_m2(gamma = 0.004, r_f = rf, x_init = x)$fixed_point()$x_star
  m_d <- model_m2(gamma = 0.004, r_f = 0.3, x_init = x)
  m_p <- model_m2(gamma = 0.004, r_f = 0.3, x_init = x, impact = "printed")
  expect_identical(m_p$params$impact, "printed")
  expect_lt(max(abs(m_p$fixed_point()$x_star - m_d$fixed_point()$x_star)), 1e-9)
  step <- simaci_input(c(-0.0002, rep(0, 5)), "step", cycle_unit = "step")
  run_d <- simulate_run(m_d, step, n_cycles = 6L, y0 = x)
  run_p <- simulate_run(m_p, step, n_cycles = 6L, y0 = x)
  expect_false(run_d$fell || run_p$fell)
  expect_gt(max(abs(run_p$events$theta_dot_post - run_d$events$theta_dot_post)), 1e-6)
  x0 <- model_m2(gamma = 0.004)$fixed_point()$x_star
  run_d0 <- simulate_run(model_m2(gamma = 0.004), step, n_cycles = 6L, y0 = x0)
  run_p0 <- simulate_run(model_m2(gamma = 0.004, impact = "printed"), step, n_cycles = 6L, y0 = x0)
  # at r_f = 0 the two maps differ only by the order of the floating-point products, a
  # difference that the dynamics amplify to about 1e-11 over six steps
  expect_lt(max(abs(run_p0$events$theta_dot_post - run_d0$events$theta_dot_post)), 1e-9)
})

test_that("M2 with r_f = 0 and k = 0 walks like the terrain reading of M1", {
  w <- walkbench:::m1_walker_env()
  fp1 <- w$find_fixed_point(0.009)
  set.seed(3)
  d <- diff(runif(31, -0.5, 0.5) * 0.0002)
  ref <- w$simulate_walker(30, 0.009, 0, fp1$y0, dt = 0.03, variant = "terrain", eps_seq = d)
  run <- simulate_run(model_m2(gamma = 0.009, t_min = 1.0), simaci_input(d, "test"),
                      n_cycles = 30, y0 = fp1$y0)
  expect_false(run$fell)
  expect_lt(max(abs(unname(run$samples) - unname(ref$samples))), 1e-8)
  fp2 <- model_m2(gamma = 0.009)$fixed_point()
  expect_lt(max(abs(fp2$x_star[1:2] - c(fp1$theta, fp1$theta_dot))), 1e-9)
  expect_lt(abs(fp2$period - fp1$period), 1e-8)
  # both eigenvalue estimates use forward differences of step 1e-7
  expect_lt(abs(max(Mod(fp2$eig)) - max(Mod(fp1$eig_step_map))), 1e-4)
})

test_that("the energy of the hip mass is conserved during a step", {
  for (r in c(0.3, 0.7)) {
    p <- list(gamma = 0.004, k = 2, r_f = r)
    y0 <- c(0.2, -0.2, 0.4, 0)
    out <- deSolve::lsoda(y0, seq(0, 2, by = 0.05), walkbench:::m2_rhs, p, rtol = 1e-12, atol = 1e-14)
    th <- out[, 2]; thd <- out[, 3]
    dd <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(th)
    e <- 0.5 * dd * thd^2 - sin(p$gamma) * (-r * th - (1 - r) * sin(th)) +
      cos(p$gamma) * (r + (1 - r) * cos(th))
    expect_lt(max(abs(e - e[1])), 1e-9)
  }
})

test_that("the global resampling gives finite states up to the end of the run", {
  # configuration and seed in which the unbounded grid ended just after the last contact
  x <- model_m2(gamma = 0.004)$fixed_point()$x_star
  for (rf in seq(0, 0.95, by = 0.05)[2:4]) {
    x <- model_m2(gamma = 0.004, r_f = rf, x_init = x)$fixed_point()$x_star
  }
  m <- model_m2(gamma = 0.004, r_f = seq(0, 0.95, by = 0.05)[4], x_init = x)
  set.seed(30000301)
  run <- simulate_run(m, simaci_input(rnorm(200) * 1e-5, "iid_gaussian"), n_cycles = 200, y0 = x)
  # post-generation fix by Claude Opus 5.5, 2026-11-06: walkbench 0.2.0 changed the primary
  # settings of M2; the global resampling is that of settings_m2("continuous")
  st <- settings_m2("continuous")
  rs <- walkbench:::resample_states(run, st$state, "global", 100, 2)
  expect_true(all(is.finite(rs$X)))
  expect_true(is.finite(estimate_divergence(run, st, 2L)$lambda_s))
  expect_true(is.finite(estimate_divergence(run, m$estimation_settings, 2L)$lambda_s))
})

test_that("the fixed point follows the foot radius and the hip stiffness by continuation", {
  x <- NULL
  for (r in c(0, 0.1, 0.2)) {
    fp <- model_m2(gamma = 0.004, r_f = r, x_init = x)$fixed_point()
    expect_lt(max(Mod(fp$eig)), 1)
    x <- fp$x_star
  }
  expect_gt(fp$step_length, model_m2(gamma = 0.004)$fixed_point()$step_length)
})
