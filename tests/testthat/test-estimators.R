# Title: Tests of the common estimators against the frozen functions of M1
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: specification, section 13.3, test-estimators.R. Short runs (120 steps, 20
#   of them transient) keep the test fast.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

short_run <- function(m1, input) {
  y0 <- m1$fixed_point()$y0 + c(1e-9, -1e-9, 0, 0)
  trim_run(simulate_run(m1, input, n_cycles = 120, y0 = y0), 20L)
}

test_that("estimate_divergence() with the M1 settings equals divergence_exponents()", {
  m1 <- model_m1()
  w <- walkbench:::m1_walker_env()
  inputs <- list(input_iid_uniform(120, 0.012, seed = 1),
                 input_arfima(120, 0, 0.003, seed = 2),
                 input_arfima(120, 0.4, 0.003, seed = 3))
  for (inp in inputs) {
    run <- short_run(m1, inp)
    tau <- mean(run$events$duration)
    ref <- w$divergence_exponents(walkbench:::as_m1_sim(run), tau)
    got <- estimate_divergence(run, m1$estimation_settings)
    expect_identical(got$lambda_s, ref$lambda_s)
    expect_identical(got$lambda_l, ref$lambda_l)
    expect_identical(got$mld, ref$mld)
    expect_identical(got$slopes, ref$slopes)
    expect_identical(got$n_pairs, ref$n_pairs)
    expect_identical(got$lambda_l_per_step, got$lambda_l * tau)
  }
})

test_that("Floquet multipliers, MeanSD and DFA equal the frozen functions", {
  m1 <- model_m1()
  w <- walkbench:::m1_walker_env()
  ref_run <- short_run(m1, simaci_input(numeric(120), "zero"))
  xstar <- reference_xstar(ref_run, m1)
  run <- short_run(m1, input_arfima(120, 0.2, 0.003, seed = 4))
  arr <- m1$normalize_cycles(run)
  expect_identical(estimate_floquet(run, m1, xstar), w$floquet_multipliers(arr, xstar))
  expect_identical(estimate_variability(run, m1)$msd, w$mean_sd(arr))
  x <- input_arfima(300, 0.3, 1, seed = 9)$values
  # post-generation fix by Claude Opus 5.5, 2026-11-06: since walkbench 0.4.0 the default DFA
  # is that of the analysis plan; the frozen DFA is called through dfa_settings("feasibility")
  fea <- dfa_settings("feasibility")
  expect_identical(estimate_dfa(x, fea), w$dfa_alpha(x)$alpha)
  expect_true(is.na(estimate_dfa(rep(1, 100), fea)))
})

# Tests added for walkbench 0.4.0 by the SP1.1 session (Claude Opus 5.5), 2026-11-06: DFA of
# the analysis plan frozen at commit ba72afe (section 5.2), that of Terrier (2026, Physica A)

test_that("the DFA of the plan equals compute_dfa_alpha() of the Physica A archive", {
  # reference values computed on 2026-11-06 with compute_dfa_alpha() of the Zenodo archive of
  # Terrier (2026) ("time series R", PhysicaA_paper/zenodo_archive_v2/01_scripts/functions/dfa.R)
  set.seed(3)
  rw <- cumsum(rnorm(500))
  xs <- list(input_arfima(300, 0.3, 1, seed = 9)$values,
             input_arfima(1200, 0, 1, seed = 21)$values,
             input_arfima(96, 0.45, 1, seed = 5)$values, rw,
             input_arfima(600, -0.3, 1, seed = 8)$values)
  ref <- c(0.656397681472, 0.519003817216, 0.951104909048, 1.257955737878, 0.200317966872)
  got <- vapply(xs, estimate_dfa, numeric(1))
  expect_equal(got, ref, tolerance = 1e-11)
  # no alpha below 96 values (decision of the author of 2026-11-06 at 17:55)
  expect_true(is.na(estimate_dfa(xs[[1]][1:95])))
  expect_false(is.na(estimate_dfa(xs[[1]][1:96])))
  expect_true(is.na(estimate_dfa(rep(1, 200))))
  # live comparison when the repository "time series R" lies next to SIM-ACI
  f <- file.path("..", "..", "..", "..", "time series R", "PhysicaA_paper",
                 "zenodo_archive_v2", "01_scripts", "functions", "dfa.R")
  skip_if_not(file.exists(f), "repository time series R not found")
  e <- new.env()
  sys.source(f, envir = e)
  for (x in xs) expect_equal(estimate_dfa(x), e$compute_dfa_alpha(as.numeric(scale(x))),
                             tolerance = 1e-12)
})

test_that("estimate_all(): DFA of the kept input, of the step and of the stride durations", {
  m1 <- model_m1()
  inp <- input_arfima(220, 0.2, 0.003, seed = 7, n_transient = 20)
  y0 <- m1$fixed_point()$y0 + c(1e-9, -1e-9, 0, 0)
  run <- trim_run(simulate_run(m1, inp, n_cycles = 220, y0 = y0), 20L)
  row <- estimate_all(run, m1, input = inp)
  expect_identical(row$alpha_input, estimate_dfa(inp$values[21:220]))
  expect_identical(row$alpha_duration, estimate_dfa(run$events$duration))
  strides <- run$events$duration[seq(1, 199, 2)] + run$events$duration[seq(2, 200, 2)]
  expect_identical(row$alpha_stride_duration, estimate_dfa(strides))
  # 100 strides: alpha is computed; the feasibility setting uses the whole input
  expect_false(is.na(row$alpha_stride_duration))
  fea <- estimate_all(run, m1, input = inp, dfa = dfa_settings("feasibility"))
  expect_identical(fea$alpha_input, estimate_dfa(inp$values, dfa_settings("feasibility")))
})

test_that("with the fixed pair set the number of pairs is the same at every horizon", {
  m1 <- model_m1()
  run <- short_run(m1, input_arfima(120, 0.2, 0.003, seed = 6))
  st <- m1$estimation_settings
  st$pairs <- "fixed"
  got <- estimate_divergence(run, st)
  nbr <- got$neighbors
  n <- nrow(run$samples)
  counts <- vapply(seq_len(got$n_horizon),
                   function(k) sum(seq_len(n) <= n - k & nbr <= n - k), integer(1))
  expect_true(all(counts == got$n_pairs))
})

test_that("estimate_all() returns one row with the columns of section 8.2", {
  m1 <- model_m1()
  run <- short_run(m1, input_arfima(120, 0.2, 0.003, seed = 7))
  row <- estimate_all(run, m1, input = input_arfima(120, 0.2, 0.003, seed = 7))
  expect_identical(nrow(row), 1L)
  expect_true(all(c("mean_duration", "msd_thG", "fm50", "lambda_s", "lambda_l",
                    "lambda_l_per_step", "n_pairs", "alpha_input", "alpha_command",
                    "alpha_duration", "alpha_stride_duration") %in% names(row)))
})
