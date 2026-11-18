# Title: Tests of the campaign rules of the frozen analysis plan (walkbench 0.4.0)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-06
# Description: rules of the analysis plan frozen at commit ba72afe (osf.io/m74tk): master
#   seeds of the seven campaigns, cells and replicates numbered from 1 with d varying fastest,
#   seed of the bootstrap (section 4.3); seed blocks of the calibration runs (sections 4.3
#   and 4.5); 21 attempts at most, fall rate, flag above 20% and inclusion at 80% (section
#   4.4); rescaling after the transient (section 4.3); circular start and surrogates
#   (section 4.8).
# Dependencies: testthat, walkbench
# Input: none
# Output: none

test_that("the seven campaigns have the master seeds 100,000,000 + 10,000,000 j", {
  tab <- campaign_table()
  expect_identical(tab$j, 1:7)
  expect_identical(tab$master, as.integer(1e8 + 1e7 * 1:7))
  expect_identical(tab$campaign[c(2, 3, 7)],
                   c("real stride series", "crossed design", "series-length runs"))
  expect_identical(campaign_master(3), 130000000L)
  expect_error(campaign_master(8), "1 to 7")
  expect_true(check_master_seeds(tab$master))
  expect_identical(bootstrap_seed(), 20261106L)
})

test_that("seed_campaign() numbers cells and replicates from 1 in a campaign", {
  m <- campaign_master(3)
  expect_identical(seed_campaign(m, 1, 1, 0), 130000101L)
  expect_identical(seed_campaign(m, 45, 20, 20), as.integer(130000000 + 2000000 + 4500 + 20))
  expect_error(seed_campaign(m, 0, 1), "numbered from 1")
  expect_error(seed_campaign(m, 1, 0), "numbered from 1")
  expect_error(seed_campaign(125000000, 1, 1), "campaign")
  expect_error(seed_campaign(m, 1, 1, 21), "0 to 20")
  # the calibration runs, with masters below 100,000,000, numbered from 0
  expect_identical(seed_campaign(50000000, 0, 0, 0), 50000000L)
})

test_that("cells are numbered from 1 with the first factor varying fastest", {
  cl <- number_cells(d = c(-0.3, 0, 0.45), sigma = c(1, 2))
  expect_identical(cl$cell, 1:6)
  expect_identical(cl$d, rep(c(-0.3, 0, 0.45), 2))
  expect_identical(cl$sigma, rep(c(1, 2), each = 3))
  expect_error(number_cells(c(1, 2)), "named")
})

test_that("check_seed_plan() keeps the calibration blocks apart from the other seeds", {
  expect_true(check_seed_plan(c(50000000, 53000000)))
  expect_true(check_seed_plan(c(40000000, 43000000, 50000000, 53000000)))
  expect_false(check_seed_plan(c(50000000, 51000000)))
  expect_false(check_seed_plan(69000000))   # covers the seeds of the surrogates
  expect_false(check_seed_plan(110000000))  # campaign 1, above the limit
  expect_false(check_seed_plan(29000000))   # replication runs of M2
  expect_false(check_seed_plan(1000000))    # feasibility study
  bad <- check_seed_plan(69000000)
  expect_identical(attr(bad, "conflicts")$b, "surrogates")
})

fake_falling_model <- function() {
  structure(list(id = "fake", label = "always falls", cycle_unit = "step",
                 default_injection = "none", params = list(dt = 0.01),
                 fixed_point = function() list(y0 = c(0, 0)),
                 simulate = function(input, n_cycles, y0, controller, metronome, injection) {
                   list(fell = TRUE, fall_cycle = 2L, n_cycles_done = 2L)
                 }),
            class = "simaci_model")
}

test_that("a trial makes 21 attempts at most and is then recorded without outcomes", {
  m <- fake_falling_model()
  dr <- function(seed) list(input = simaci_input(numeric(60), "zero"), y0 = c(0, 0))
  tr <- run_trial(m, dr, seed_of = function(a) seed_campaign(campaign_master(3), 1, 1, a),
                  n_transient = 0L, n_keep = 50L)
  expect_true(tr$fell)
  expect_identical(length(tr$seeds), 21L)
  expect_identical(tr$n_redraws, 20L)
  expect_identical(tr$seeds[21], 132000101)
  # a fixed input is not redrawn
  fx <- draw_fixed(m, input_fixed_series(1:60, 0L, 1))
  tr2 <- run_trial(m, fx, seed_of = function(a) a + 1, n_transient = 0L, n_keep = 50L)
  expect_identical(length(tr2$seeds), 1L)
  rows <- bind_trials(list(tr, tr2))
  expect_identical(rows$attempts, c(21L, 1L))
})

test_that("summarize_cells() applies the rules of section 4.4", {
  rows <- data.frame(cell = rep(1:2, each = 5), replicate = rep(1:5, 2),
                     fell = c(FALSE, FALSE, FALSE, FALSE, TRUE, rep(FALSE, 5)),
                     n_redraws = c(0, 1, 0, 0, 20, 0, 0, 0, 0, 3))
  s <- summarize_cells(rows, planned = 5)
  # cell 1: 4 of 5 replicates with outcomes, 26 attempts and 22 falls; cell 2: 8 and 3
  expect_identical(s$with_outcomes, c(4L, 5L))
  expect_equal(s$attempts, c(26, 8))
  expect_equal(s$falls, c(22, 3))
  expect_equal(s$fall_rate, c(22 / 26, 3 / 8))
  expect_identical(s$flag_fall_rate, c(TRUE, TRUE))
  expect_identical(s$included, c(TRUE, TRUE))
  # with 10 planned replicates the first cell has 40% of them and is excluded
  s10 <- summarize_cells(rows, data.frame(cell = 1:2, planned = c(10, 5)))
  expect_identical(s10$included, c(FALSE, TRUE))
  expect_identical(summarize_cells(rows[rows$cell == 2, ], 5)$flag_fall_rate, TRUE)
})

test_that("input_arfima() rescales the values after the transient", {
  x0 <- input_arfima(634, 0.45, 2, seed = 11)$values
  x1 <- input_arfima(634, 0.45, 2, seed = 11, n_transient = 34)$values
  kept <- x1[35:634]
  expect_equal(mean(kept), 0, tolerance = 1e-12)
  expect_equal(sd(kept), 2, tolerance = 1e-12)
  expect_equal(mean(x0), 0, tolerance = 1e-12)
  expect_equal(sd(x0), 2, tolerance = 1e-12)
  # one realization under two affine maps
  expect_equal(cor(x0, x1), 1, tolerance = 1e-12)
  # the random stream advances by the same draws
  set.seed(5)
  invisible(input_arfima(100, 0.2, 1))
  r1 <- rnorm(1)
  set.seed(5)
  invisible(input_arfima(100, 0.2, 1, n_transient = 20))
  expect_identical(rnorm(1), r1)
  expect_error(input_arfima(100, 0, 1, n_transient = 99), "n_transient")
})

test_that("input_fixed_series() rescales the window and gives a circular start", {
  v <- c(3, 1, 4, 1, 5, 9, 2, 6)
  inp <- input_fixed_series(v, n_transient = 3, sd = 0.5)
  z <- (v - mean(v)) / sd(v) * 0.5
  expect_identical(inp$values, c(z[6:8], z))
  expect_false(inp$redrawable)
  expect_equal(sd(inp$values[4:11]), 0.5)
  expect_error(input_fixed_series(v, 9, 1), "n_transient")
})

test_that("surrogates follow the seed rule of section 4.8", {
  expect_identical(seed_surrogate(1, 1), 70001001L)
  expect_identical(seed_surrogate(60, 11), 70060011L)
  expect_error(seed_surrogate(0, 1), "s must")
  v <- sin(1:50) + (1:50) / 10
  a <- input_surrogate(v, s = 2, i = 3, n_transient = 5, sd = 1)
  b <- input_surrogate(v, s = 2, i = 3, n_transient = 5, sd = 1)
  expect_identical(a$values, b$values)
  expect_identical(a$seed, 70002003L)
  expect_identical(a$source, "surrogate")
  # a permutation keeps the values of the window
  expect_equal(sort(a$values[6:55]), sort(input_fixed_series(v, 0L, 1)$values))
  expect_false(identical(input_surrogate(v, 2, 11, 5, 1)$values, a$values))
})
