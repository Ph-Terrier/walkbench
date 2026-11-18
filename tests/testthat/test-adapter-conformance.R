# Title: Conformance test run on every model adapter of the package
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: specification, sections 3, 12 and 13.3. A new adapter (M2 to M4) passes this
#   test before the replication runs of its sub-project.
# Dependencies: testthat, walkbench
# Input: none
# Output: none

members <- c("id", "label", "version", "params", "time_unit", "cycle_unit",
             "cycles_per_stride", "state_names", "reduced_state_names", "injection_points",
             "default_injection", "fixed_point", "post_contact_state", "cycle_map", "flow",
             "rhs", "coord_maps", "simulate", "normalize_cycles", "corrected_index",
             "estimation_settings", "fall_rule")
fun_members <- c("fixed_point", "post_contact_state", "cycle_map", "flow", "rhs",
                 "simulate", "normalize_cycles")

for (m in list_models()) {
  test_that(paste("adapter", m$id, "conforms to the interface"), {
    expect_s3_class(m, "simaci_model")
    expect_true(all(members %in% names(m)))
    expect_true(all(vapply(m[fun_members], is.function, TRUE)))
    expect_true(m$cycle_unit %in% c("step", "stride"))
    expect_true(isTRUE(m$injection_points[[m$default_injection]]$linearizable))
    fp <- m$fixed_point()
    expect_length(fp$x_star, length(m$reduced_state_names))
    # eigenvalues of the cycle map, for the rule of the transient (walkbench 0.3.0)
    expect_true(length(fp$eig) >= 1L && all(is.finite(Mod(fp$eig))))
    expect_gte(transient_cycles(m), 20L)
    # unperturbed run on the limit cycle
    run <- simulate_run(m, simaci_input(numeric(40), "zero", cycle_unit = m$cycle_unit),
                        n_cycles = 40)
    expect_false(run$fell)
    expect_lt(stats::sd(run$events$duration), 1e-8)
    expect_true(all(m$state_names %in% colnames(run$samples)))
    # one controller call per cycle
    calls <- 0L
    ctrl <- function(k, info) {
      calls <<- calls + 1L
      list(u = 0, u_exo = 0, u_corr = 0, deviation = NA_real_)
    }
    simulate_run(m, n_cycles = 15, controller = ctrl)
    expect_identical(calls, 15L)
    # the same seed gives the same run
    draw <- draw_single_seed(m, function(n, seed) input_arfima(n, 0.2, 0.003, seed = seed,
                                                                cycle_unit = m$cycle_unit), 30)
    d1 <- draw(123)
    d2 <- draw(123)
    r1 <- simulate_run(m, d1$input, n_cycles = 30, y0 = d1$y0)
    r2 <- simulate_run(m, d2$input, n_cycles = 30, y0 = d2$y0)
    expect_identical(r1$samples, r2$samples)
    # cycle units of input and model agree
    other <- if (m$cycle_unit == "step") "stride" else "step"
    expect_error(simulate_run(m, simaci_input(numeric(10), "x", cycle_unit = other),
                              n_cycles = 10), "cycle unit")
  })
}

test_that("the seed rule gives distinct seeds and checks its ranges", {
  s <- outer(0:999, 0:99, function(c, r) seed_campaign(1000, c, r))
  expect_identical(length(unique(as.vector(s))), length(s))
  expect_identical(seed_campaign(1000, 0, 0, 1), 101000L)
  expect_error(seed_campaign(1000, 1000, 0), "cell")
  expect_error(seed_campaign(1000, 0, 100), "replicate")
  expect_true(check_master_seeds(c(1000, 2101000)))
  expect_false(check_master_seeds(c(1000, 2000000)))
})
