# Title: Tests of the cycle loop of M1 against the frozen simulate_walker()
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: specification, section 13.3, test-m1-simulator.R. The reference is the frozen
#   simulate_walker() of the pinned walker file; trim_sim_frozen() below is a verbatim copy
#   of trim_sim() of feasibility/scripts/02_run_experiment.R (lines 104 to 120), which the
#   pinned walker file does not contain.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

trim_sim_frozen <- function(sim, n_discard, n_keep) {
  ev <- sim$events
  keep_steps <- (n_discard + 1):(n_discard + n_keep)
  t0 <- ev$t_start[keep_steps[1]]
  smp <- sim$samples
  sel <- smp[, "step"] %in% keep_steps
  smp <- smp[sel, , drop = FALSE]
  smp[, "t"] <- smp[, "t"] - t0
  smp[, "step"] <- smp[, "step"] - n_discard
  ev <- ev[keep_steps, , drop = FALSE]
  ev$t_start <- ev$t_start - t0
  ev$t_end <- ev$t_end - t0
  ev$step <- ev$step - n_discard
  list(samples = smp, events = ev,
       bounds = sim$bounds[keep_steps],
       fell = sim$fell, n_steps_done = n_keep, dt = sim$dt, rho = sim$rho)
}

strip <- function(b) lapply(b, function(x) lapply(x, unname))

test_that("the cycle loop gives the values of simulate_walker() at every injection point", {
  m1 <- model_m1()
  w <- walkbench:::m1_walker_env()
  fp <- m1$fixed_point()
  y0 <- fp$y0 + c(1e-9, -1e-9, 0, 0)
  for (inj in names(m1$injection_points)) {
    v <- m1$injection_points[[inj]]$variant
    set.seed(11)
    eps <- stats::runif(30, -0.5, 0.5) * if (v == "terrain") 0.0004 else 0.004
    ref <- w$simulate_walker(30, 0.009, 0, y0, dt = 0.03, variant = v, eps_seq = eps)
    run <- simulate_run(m1, simaci_input(eps, "test"), n_cycles = 30, y0 = y0,
                        injection = inj)
    expect_identical(run$fell, ref$fell)
    expect_identical(unname(run$samples), unname(ref$samples))
    expect_identical(run$events$cycle, ref$events$step)
    expect_identical(run$events$t_start, ref$events$t_start)
    expect_identical(run$events$t_end, ref$events$t_end)
    expect_identical(run$events$duration, ref$events$duration)
    expect_identical(run$events$u, ref$events$eps)
    expect_identical(run$events$leg, ref$events$stance)
    expect_identical(strip(run$bounds), strip(ref$bounds[seq_len(run$n_cycles_done)]))
  }
})

test_that("the internal uniform draw of simulate_walker() equals input_iid_uniform()", {
  m1 <- model_m1()
  w <- walkbench:::m1_walker_env()
  y0 <- m1$fixed_point()$y0
  ref <- w$simulate_walker(25, 0.009, 0.008, y0, dt = 0.03, seed = 77, variant = "timing")
  run <- simulate_run(m1, input_iid_uniform(25, 0.008, seed = 77), n_cycles = 25, y0 = y0)
  expect_identical(run$events$u, ref$events$eps)
  expect_identical(unname(run$samples), unname(ref$samples))
})

test_that("trim_run() repeats trim_sim() of the feasibility scripts", {
  m1 <- model_m1()
  y0 <- m1$fixed_point()$y0 + c(1e-9, 0, 0, 0)
  inp <- input_arfima(40, 0.3, 0.003, seed = 5)
  run <- simulate_run(m1, inp, n_cycles = 40, y0 = y0)
  tr <- trim_run(run, 10L)
  ref <- trim_sim_frozen(walkbench:::as_m1_sim(run), 10L, 30L)
  expect_identical(unname(tr$samples), unname(ref$samples))
  expect_identical(tr$events$t_start, ref$events$t_start)
  expect_identical(tr$events$t_end, ref$events$t_end)
  expect_identical(tr$events$cycle, ref$events$step)
  expect_identical(strip(tr$bounds), strip(ref$bounds))
})

test_that("an unperturbed run of 600 steps stays on the limit cycle", {
  m1 <- model_m1()
  run <- simulate_run(m1, simaci_input(numeric(600), "zero"), n_cycles = 600)
  expect_false(run$fell)
  expect_identical(run$n_cycles_done, 600L)
  expect_lt(stats::sd(run$events$duration), 1e-8)
})

test_that("a fall sets the flag and keeps the completed cycles", {
  m1 <- model_m1()
  run <- simulate_run(m1, simaci_input(rep(0.2, 20), "large"), n_cycles = 20)
  expect_true(run$fell)
  expect_true(is.finite(run$fall_cycle))
  expect_identical(nrow(run$events), run$n_cycles_done)
  expect_lt(run$n_cycles_done, 20L)
})

test_that("a too short input or another cycle unit stops with an error", {
  m1 <- model_m1()
  expect_error(simulate_run(m1, simaci_input(numeric(5), "short"), n_cycles = 10),
               "too short")
  expect_error(simulate_run(m1, simaci_input(numeric(10), "x", cycle_unit = "stride"),
                            n_cycles = 10), "cycle unit")
})
