# Title: Tests of the primary estimation settings of model M2 (walkbench 0.2.0)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-06
# Description: settings_m2() against table 5.1 of the analysis plan, draft 0.4 (approved by the
#   author on 2026-11-06 at 12:26): state variables of the model referred to the stance leg,
#   each step interpolated separately on the global grid of 100 samples per stride, neighbors
#   excluded within 50 samples, mean of the ln distances over a shrinking set of pairs,
#   least-squares slopes. The resampling and the curve are checked against an independent
#   computation written below, which follows resample_by_step() and to_model_vars() of
#   SIMACI/wp1_replication/06_m2_estimation_sensitivity.R.
#   walkbench 0.7.0 (2026-11-20, SP1.1 session, Claude Opus 5.5): the independent
#   computation counts its grid points from the number of steps, as resample_states() now
#   does, and a test checks on a constructed run that the grid of "global" and
#   "global_by_cycle" ends at the end of the run when the count from the times falls just
#   below the whole number.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

# short noisy run of M2 with both elements of the model active
m2_test_run <- function(n_cycles = 60L) {
  m <- model_m2(gamma = 0.004, r_f = 0.3, k = 2)
  x <- m$fixed_point()$x_star
  set.seed(20261106)
  run <- simulate_run(m, simaci_input(rnorm(n_cycles) * 1e-5, "iid_gaussian"),
                      n_cycles = n_cycles, y0 = x)
  list(model = m, run = run)
}

# independent computation: model variables by explicit formulas, then each grid point
# interpolated between the states of its own step
independent_resampling <- function(run, n_per = 100) {
  ev <- run$events
  smp <- run$samples
  vars <- function(m, leg) {
    if (leg == 1L) cbind(m[, 1], m[, 2], -m[, 3], -m[, 4]) else
      cbind(m[, 1] + m[, 3], m[, 2] + m[, 4], m[, 3], m[, 4])
  }
  cs <- c("thG", "thGd", "phG", "phGd")
  t0 <- ev$t_start[1]
  t1 <- ev$t_end[nrow(ev)]
  mean_stride <- 2 * mean(ev$duration)
  # walkbench 0.7.0: n_per points per stride from the number of steps, end of the run included
  n_out <- nrow(ev) * n_per / 2 + 1
  grid <- pmin(t0 + (seq_len(n_out) - 1) * mean_stride / n_per, t1)
  X <- matrix(NA_real_, n_out, 4L)
  for (g in seq_len(n_out)) {
    k <- which(ev$t_start <= grid[g] & (grid[g] < ev$t_end | seq_len(nrow(ev)) == nrow(ev)))
    k <- max(k)
    idx <- which(smp[, "cycle"] == ev$cycle[k])
    tt <- c(ev$t_start[k], smp[idx, "t"], ev$t_end[k])
    yy <- vars(rbind(run$bounds[[k]]$start[1, cs], smp[idx, cs, drop = FALSE],
                     run$bounds[[k]]$end[1, cs]), ev$leg[k])
    X[g, ] <- vapply(1:4, function(v) stats::approx(tt, yy[, v], xout = grid[g], ties = mean)$y,
                     numeric(1))
  }
  X
}

test_that("settings_m2() gives the settings of table 5.1 of the plan, draft 0.4", {
  st <- settings_m2()
  expect_identical(model_m2()$estimation_settings, st)
  expect_identical(st$state, c("theta", "theta_dot", "phi", "phi_dot"))
  expect_identical(st$coordinates, "model")
  expect_null(st$embedding)
  expect_identical(st$resample, list(method = "global_by_cycle", n_per = 100, per = "stride"))
  expect_identical(st$window_unit, "stride")
  expect_equal(st$theiler, 0.5)
  expect_equal(st$window_s, c(0, 0.5))
  expect_equal(st$window_l, c(4, 10))
  expect_equal(st$horizon, 10)
  expect_identical(st$slope, "lsq")
  expect_identical(st$smoothing, "none")
  expect_identical(st$pairs, "shrinking")
  # the settings of draft 0.3, primary in walkbench 0.1.x, are unchanged
  old <- list(state = c("thG", "thGd", "phG", "phGd"), embedding = NULL,
              resample = list(method = "global", n_per = 100, per = "stride"),
              window_unit = "stride", theiler = 1, horizon = 10, window_s = c(0, 0.5),
              window_l = c(4, 10), slope = "lsq", smoothing = "none", pairs = "shrinking")
  expect_identical(settings_m2("continuous"), old)
})

test_that("m2_model_coords() inverts m2_leg_coords() for both stance legs", {
  set.seed(3)
  y <- matrix(runif(40, -1, 1), 10, 4)
  for (leg in 1:2) {
    back <- walkbench:::m2_model_coords(walkbench:::m2_leg_coords(y, leg), leg)
    expect_lt(max(abs(unname(back) - y)), 1e-15)
    maps <- walkbench:::m2_model_coordinate_maps()$maps
    expect_identical(unname(walkbench:::m2_leg_coords(y, leg) %*% t(maps[[leg]])),
                     unname(walkbench:::m2_model_coords(walkbench:::m2_leg_coords(y, leg), leg)))
  }
})

test_that("the recovered state variables equal the post-strike states of the loop", {
  tr <- m2_test_run(30L)
  run <- tr$run
  ev <- run$events
  post <- t(vapply(seq_len(nrow(ev)), function(k) {
    walkbench:::m2_model_coords(run$bounds[[k]]$start[1, c("thG", "thGd", "phG", "phGd"),
                                                       drop = FALSE], ev$leg[k])[1, ]
  }, numeric(4)))
  ref <- as.matrix(ev[, c("theta_post", "theta_dot_post", "phi_post", "phi_dot_post")])
  expect_lt(max(abs(unname(post) - unname(ref))), 1e-15)
})

test_that("the grid of the global resamplings ends at the end of the run (walkbench 0.7.0)", {
  # constructed run of 29 contiguous cycles; the state q = sin(t) is known at every time
  n_cyc <- 29L
  dur <- 0.9 + 0.1 * sin(seq_len(n_cyc))
  t_end <- cumsum(dur)
  t_start <- c(0, t_end[-n_cyc])
  t_mid <- (t_start + t_end) / 2
  q_at <- function(t) matrix(sin(t), 1, dimnames = list(NULL, "q"))
  run <- list(samples = cbind(t = t_mid, cycle = seq_len(n_cyc), q = sin(t_mid)),
              events = data.frame(cycle = seq_len(n_cyc), t_start = t_start, t_end = t_end,
                                  duration = dur),
              bounds = lapply(seq_len(n_cyc), function(k) list(start = q_at(t_start[k]),
                                                               end = q_at(t_end[k]))))
  for (cpu in c(1, 2)) {
    whole <- n_cyc * 100 / cpu
    # with these durations, the count of walkbench 0.6.2, from the times, lost the last point
    expect_lt(floor((t_end[n_cyc] - t_start[1]) / (mean(dur) * cpu) * 100), whole)
    for (method in c("global", "global_by_cycle")) {
      rs <- walkbench:::resample_states(run, "q", method, 100, cpu)
      expect_identical(nrow(rs$X), as.integer(whole + 1))
      expect_lt(abs(rs$X[nrow(rs$X), 1] - sin(t_end[n_cyc])), 1e-9)
      expect_identical(rs$X[1, 1], sin(t_start[1]))
    }
  }
})

test_that("each grid point is interpolated inside its own step", {
  tr <- m2_test_run()
  run <- tr$run
  st <- settings_m2()
  rs <- walkbench:::resample_states(run, st$state, "global_by_cycle", 100, 2,
                                    coords = st$model_coordinates)
  ind <- independent_resampling(run)
  expect_identical(dim(rs$X), dim(ind))
  expect_lt(max(abs(rs$X - ind)), 1e-15)
  expect_equal(rs$dt, 0.01)
  # the grid is that of the global resampling
  rg <- walkbench:::resample_states(run, settings_m2("continuous")$state, "global", 100, 2)
  expect_identical(nrow(rg$X), nrow(rs$X))
  # theta jumps at the strikes: from about -theta* to about +theta* within one grid interval
  th_star <- tr$model$fixed_point()$x_star[1]
  expect_gt(max(diff(rs$X[, 1])), 1.5 * th_star)
  # a subset of the variables keeps its values
  sub <- walkbench:::resample_states(run, c("phi", "theta"), "global_by_cycle", 100, 2,
                                     coords = st$model_coordinates)
  expect_identical(sub$X, rs$X[, c(3, 1)])
})

test_that("estimate_divergence() with the primary settings equals a direct computation", {
  tr <- m2_test_run()
  run <- tr$run
  got <- estimate_divergence(run, settings_m2(), 2L)
  expect_identical(got$theiler, 50)
  expect_identical(got$n_horizon, 1000)
  X <- independent_resampling(run)
  racing <- load_racing()
  nbr <- racing$find_nearest_neighbors(X, mean_period = 50L)
  expect_true(all(abs(nbr - seq_len(nrow(X))) > 50))
  d0 <- sqrt(rowSums((X - X[nbr, , drop = FALSE])^2))
  mld <- c(mean(log(d0[d0 > 0])), racing$compute_divergence(X, nbr, max_iter = 1000))
  expect_lt(max(abs(got$mld - mld), na.rm = TRUE), 1e-12)
  tt <- (0:1000) * 0.01
  fit <- function(sel) unname(stats::lm.fit(cbind(1, tt[sel]), mld[sel])$coefficients[2])
  expect_lt(abs(got$lambda_s - fit(tt <= 0.5)), 1e-10)
  expect_lt(abs(got$lambda_l - fit(tt >= 4 & tt <= 10)), 1e-10)
  expect_identical(got$unit, "per stride")
  expect_equal(got$lambda_l_per_step, got$lambda_l * 0.5)
  row <- estimate_all(run, tr$model)
  expect_equal(row$lambda_l, got$lambda_l)
})

test_that("the state variables of the model need the resampling of each cycle", {
  tr <- m2_test_run(30L)
  st <- settings_m2()
  st$resample$method <- "global"
  expect_error(estimate_divergence(tr$run, st, 2L), "global_by_cycle")
  st$resample$method <- "none"
  expect_error(estimate_divergence(tr$run, st, 2L), "need a resampling")
  st <- settings_m2()
  st$state <- c("theta", "psi")
  expect_error(estimate_divergence(tr$run, st, 2L), "unknown state variables")
})
