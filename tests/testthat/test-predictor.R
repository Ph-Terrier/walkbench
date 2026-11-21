# Title: Tests of the analytic predictor against the pinned SPINAL script 07b (walkbench 0.5.0)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-06
# Description: predict_spinal_levels() with an FI(d) input centered over 620 values, 600 kept
#   steps and the linearization of M1 must return the values of predict_levels() of the pinned
#   copy of time series R/SPINAL/validation-aci/checks/07b_walker_prediction.R (commit
#   ced76be). The pinned file is evaluated unchanged: only its definitions, up to that of
#   predict_levels(), are evaluated, in an environment that holds the linearization of M1 from
#   linearize() (identical to 07a_walker_linearization.rds, criterion (b) of SP1.1) and the FI
#   autocovariance of the certified equation EQ-18. Its output section, which writes into the
#   repository of origin, is not evaluated. The values printed by 07b
#   (07b_walker_prediction_results.txt) and the matched FI levels printed by
#   10_ar1_against_fi_results.txt are checked as well.
#   walkbench 0.7.0 (2026-11-20, SP1.1 session, Claude Opus 5.5): the aliases predictor_*()
#   are the same functions as the former names.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

lin_m1_07b <- function() {
  lin <- linearize(model_m1())
  lin$n_steps <- 600L
  lin$sd_eps <- 0.003
  lin
}

# environment with the definitions of the pinned 07b, evaluated in their order of the file
spinal_07b_env <- function(lin) {
  f <- pinned_file("time_series_R/SPINAL/validation-aci/checks/07b_walker_prediction.R")
  ex <- parse(f, keep.source = FALSE)
  keep <- c("fi_acf", "e_ln_quadratic_form", "n_past", "k_max", "n_e", "n_series", "n_steps",
            "p_k", "q_k", "p0", "ell_idx", "d_grid", "d_all", "near", "predict_levels")
  target <- function(e) {
    x <- e[[2]]
    while (is.call(x)) x <- x[[2]]
    as.character(x)
  }
  env <- new.env(parent = asNamespace("stats"))
  env$lin <- lin
  # FI autocovariance of EQ-18 (gamma_0 = Gamma(1 - 2d) / Gamma(1 - d)^2, then the product of
  # fi_rho_product()); fi_acf() of 07b divides it by gamma_0
  env$eq18_fi_acvf <- function(d, n_lags) {
    g0 <- gamma(1 - 2 * d) / gamma(1 - d)^2
    i <- seq_len(n_lags - 1L)
    c(g0, g0 * cumprod((i - 1 + d) / (i - d)))
  }
  for (e in ex) {
    is_assign <- is.call(e) && identical(e[[1]], as.name("<-"))
    is_for <- is.call(e) && identical(e[[1]], as.name("for"))
    if ((is_assign && target(e) %in% keep) || is_for) eval(e, env)
    if (is_assign && identical(target(e), "predict_levels")) break
  }
  env
}

test_that("autocorrelations and autocovariances of the inputs", {
  expect_equal(fi_autocorrelation(0, 5), c(1, 0, 0, 0, 0))
  r <- fi_autocorrelation(0.3, 4)
  expect_equal(r[2], 0.3 / 0.7)
  expect_equal(r[3], r[2] * 1.3 / 1.7)
  expect_equal(ar1_autocorrelation(0.5, 4), c(1, 0.5, 0.25, 0.125))
  cg <- centered_autocovariance(fi_autocorrelation(0, 620), 620, 2)
  expect_equal(cg$vbar, 1 / 620)
  set.seed(1)
  x <- rnorm(200)
  expect_equal(sample_autocovariance(x, 10),
               as.numeric(stats::acf(x, lag.max = 9, type = "covariance", plot = FALSE)$acf))
})

test_that("predict_spinal_levels() returns the values of the pinned 07b (M1, FI inputs)", {
  lin <- lin_m1_07b()
  env <- spinal_07b_env(lin)
  expect_true(is.function(env$predict_levels))
  for (d in c(-0.30, 0, 0.45)) {
    ref <- env$predict_levels(d)
    cg <- centered_autocovariance(fi_autocorrelation(d, 620L + 700L), 620L, 0.003)
    got <- predict_spinal_levels(lin, cg$gam, n_steps = 600L)
    expect_equal(unname(got$levels), unname(ref[c("y2", "y3", "y4")]),
                 tolerance = 1e-12)
    expect_equal(spinal_lambda_l_m1(got), unname(ref["lambda_l"]), tolerance = 1e-12)
    expect_equal(cg$vbar, unname(ref["vbar"]), tolerance = 1e-14)
  }
})

test_that("the predicted lambda_L of M1 equals the values printed by 07b", {
  lin <- lin_m1_07b()
  printed <- c("-0.3" = 0.0056, "0" = 0.0161, "0.45" = 0.0438)
  for (d in c(-0.30, 0, 0.45)) {
    cg <- centered_autocovariance(fi_autocorrelation(d, 1320L), 620L, 0.003)
    lam <- spinal_lambda_l_m1(predict_spinal_levels(lin, cg$gam, n_steps = 600L))
    expect_equal(round(lam, 4), unname(printed[as.character(d)]))
  }
})

test_that("options of update 1: maps, separations, durations and the slope per stride", {
  lin <- lin_m1_07b()
  cg <- centered_autocovariance(fi_autocorrelation(0.2, 1320L), 620L, 0.003)
  # explicit maps equal to those of 07b give the same levels as the default
  a <- predict_spinal_levels(lin, cg$gam, n_steps = 600L)
  b <- predict_spinal_levels(lin, cg$gam, n_steps = 600L, maps = list(lin$m1, lin$m2))
  expect_identical(a$levels, b$levels)
  # a map counted twice gives the level of that map alone
  one <- predict_spinal_levels(lin, cg$gam, n_steps = 600L, maps = list(lin$m1))
  two <- predict_spinal_levels(lin, cg$gam, n_steps = 600L, maps = list(lin$m1, lin$m1))
  expect_equal(two$levels, one$levels, tolerance = 1e-14)
  # odd and even separations, and the term of the durations alone
  odd <- predict_spinal_levels(lin, cg$gam, n_steps = 600L, separation_step = 1L)
  dur <- predict_spinal_levels(lin, cg$gam, n_steps = 600L, terms = "durations")
  expect_true(all(is.finite(c(odd$levels, dur$levels))))
  expect_false(isTRUE(all.equal(odd$levels, a$levels)))
  # E ln(Z' Sigma Z) of a matrix of rank 1, c v v', is ln(c |v|^2) + E ln chi2(1)
  expect_equal(walkbench:::e_ln_quadratic_form(diag(c(2, 0, 0))),
               log(2) + digamma(0.5) + log(2), tolerance = 1e-6)
  # least-squares slope of the levels, times 2
  fake <- list(levels = stats::setNames(0.3 + 0.01 * (8:20), paste0("k=", 8:20)))
  expect_equal(spinal_lambda_l_lsq(fake), 0.02)
})

test_that("settings of M2 under update 1: maps, grid, past inputs and lags", {
  m2 <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
                 x_init = c(0.171841406, -0.274656028, 0.343682812, -0.008800993))
  lin2 <- linearize(m2)
  # the default maps are those of M1 only
  expect_error(predict_spinal_levels(lin2, numeric(2000), n_steps = 600L), "maps explicitly")
  expect_error(spinal_settings_m2(lin_m1_07b(), 600L), "M2")
  # rule E4: 0.923^n <= 1e-4 from n = 115; the norm of J^n b reaches 1e-4 |b| at n = 124
  n <- spinal_n_past(lin2)
  expect_identical(n, 124L)
  jb <- function(k) {
    v <- lin2$b_vec
    for (i in seq_len(k)) v <- lin2$j_map %*% v
    sqrt(sum(v^2)) / sqrt(sum(lin2$b_vec^2))
  }
  expect_lte(jb(124), 1e-4)
  expect_gt(jb(123), 1e-4)
  st600 <- spinal_settings_m2(lin2, 600L)
  st1200 <- spinal_settings_m2(lin2, 1200L)
  expect_identical(st600$d_grid, c(2:20, 30L, 50L, 80L, 120L, 200L, 300L, 400L, 500L))
  expect_identical(st1200$d_grid, c(st600$d_grid, 700L, 900L, 1000L))
  expect_identical(st1200$d_grid_sensitivity, st600$d_grid)
  expect_identical(st1200$k_levels, 8:20)
  expect_true(all(vapply(st1200$maps, function(m) identical(m, diag(4)), logical(1))))
  # a record of 1,200 steps gives lags up to 1,199: a node at 1,000 fits, one at 1,100 does not
  set.seed(4)
  x <- rnorm(1200)
  gam <- sample_autocovariance(x, 1200L)
  expect_error(predict_spinal_levels(lin2, gam, n_steps = 1200L, k_levels = 8:20,
                                     d_grid = c(st600$d_grid, 1100L), n_past = n,
                                     maps = st1200$maps, separation_step = 1L),
               "gam needs more lags")
  expect_gte(length(gam), n + 20L + 1000L)
})

test_that("matching on the subspace that the input reaches (option initial_rank_tol)", {
  # M1: the input reaches both directions of the reduced state; the option changes nothing
  lin <- lin_m1_07b()
  cg <- centered_autocovariance(fi_autocorrelation(0.3, 1320L), 620L, 0.003)
  a <- predict_spinal_levels(lin, cg$gam, n_steps = 600L)
  b <- predict_spinal_levels(lin, cg$gam, n_steps = 600L, initial_rank_tol = 1e-8)
  expect_identical(b$initial_rank, 2L)
  expect_equal(b$levels, a$levels, tolerance = 1e-10)
  # M2: J has rank 2 and the input reaches three of the four directions after contact.
  # Test input: an AR(1) series with phi = 0.2 and a unit standard deviation, which is no cell
  # of the plan; no level and no lambda_L of M2 is printed or archived.
  m2 <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
                 x_init = c(0.171841406, -0.274656028, 0.343682812, -0.008800993))
  lin2 <- linearize(m2)
  st <- spinal_settings_m2(lin2, 600L)
  gam <- centered_autocovariance(ar1_autocorrelation(0.2, 1300L), 600L, 1)$gam
  args <- list(lin = lin2, gam = gam, n_steps = 600L, k_levels = 8:9, d_grid = c(2L, 50L),
               n_past = st$n_past, maps = st$maps, separation_step = 1L)
  expect_error(suppressWarnings(do.call(predict_spinal_levels, args)), "singul")
  p <- do.call(predict_spinal_levels, c(args, list(initial_rank_tol = 1e-8)))
  expect_identical(p$initial_rank, 3L)
  expect_true(all(is.finite(p$levels)))
})

# test added on 2026-11-07 by the SP1.1 session (Claude Opus 5.5), finding 1 of the review of
# the draft of update 2 of the registration
test_that("the settings of M2 carry the matching of update 2, which no longer stops", {
  m2 <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
                 x_init = c(0.171841406, -0.274656028, 0.343682812, -0.008800993))
  lin2 <- linearize(m2)
  for (n in c(600L, 1200L)) {
    expect_identical(spinal_settings_m2(lin2, n)$initial_rank_tol, 1e-8)
  }
  st <- spinal_settings_m2(lin2, 600L)
  # test input: AR(1) with phi = 0.2 and a unit standard deviation, no cell of the plan
  gam <- centered_autocovariance(ar1_autocorrelation(0.2, 1300L), 600L, 1)$gam
  for (matched in c(TRUE, FALSE)) {
    for (terms in c("both", "durations")) {
      expect_no_error(predict_spinal_levels(
        lin2, gam, n_steps = 600L, matched = matched, k_levels = 8:9, d_grid = c(2L, 50L),
        n_past = st$n_past, maps = st$maps, separation_step = st$separation_step,
        terms = terms, initial_rank_tol = st$initial_rank_tol))
    }
  }
  p <- predict_spinal_levels(lin2, gam, n_steps = 600L, k_levels = 8:9, d_grid = c(2L, 50L),
                             n_past = st$n_past, maps = st$maps,
                             separation_step = st$separation_step,
                             initial_rank_tol = st$initial_rank_tol)
  expect_identical(p$initial_rank, 3L)
  expect_true(all(is.finite(p$levels)))
})

test_that("window slopes and matched FI levels reproduce 10_ar1_against_fi of SPINAL", {
  # white noise: slope 0.1733 over 2 to 4 cycles and 0.0752 over 4 to 10 cycles
  expect_equal(round(window_slope_half_log_v(fi_autocorrelation(0, 21), 2:4), 4), 0.1733)
  expect_equal(round(window_slope_half_log_v(fi_autocorrelation(0, 21), 4:10), 4), 0.0752)
  got <- vapply(c(-0.3, 0.3, 0.5, 0.8), match_fi_to_ar1, numeric(1), k_window = 2:4)
  expect_equal(round(got, 3), c(-0.099, 0.145, 0.249, 0.403))
})

test_that("the aliases predictor_*() are the functions of the former names (walkbench 0.7.0)", {
  expect_identical(predictor_levels, predict_spinal_levels)
  expect_identical(predictor_d_grid, spinal_d_grid)
  expect_identical(predictor_lambda_l_m1, spinal_lambda_l_m1)
  expect_identical(predictor_lambda_l_lsq, spinal_lambda_l_lsq)
  expect_identical(predictor_n_past, spinal_n_past)
  expect_identical(predictor_settings_m2, spinal_settings_m2)
  expect_true(all(c("predictor_levels", "predictor_d_grid", "predictor_lambda_l_m1",
                    "predictor_lambda_l_lsq", "predictor_n_past", "predictor_settings_m2") %in%
                    getNamespaceExports("walkbench")))
})
