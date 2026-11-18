# Title: Tests of the outputs of the campaigns: output folder, fixed columns, run log (walkbench 0.6.2)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model, on the task of the coordination session (Claude Opus 5.5) that
#   followed the approval by the author of the triage of the review (2026-11-07, 08:41,
#   register SIMACI/docs/author_decisions.md)
# Date: 2026-11-07
# Description: findings F2, F4, F7 and F10 of the review of the code of campaigns 2 and 3 of M1
#   by a verification sub-agent (Claude Fable 5.1); triage in
#   SIMACI/docs/reviews/2026-11-07_triage_campaigns_m1.md. check_output_folder() refuses a
#   folder that is not empty, unless overwrite = TRUE, and a folder inside a forbidden folder,
#   without creating any folder. The summary row of a trial without outcomes has the columns
#   of a completed trial, empty, and estimate_all() of a run that fell has them too.
#   write_campaign_log() records the kinds of the random number generator, the versions of
#   pkgload and parallel, and the status of the whole repository.
# Dependencies: testthat, walkbench, jsonlite
# Input: none
# Output: temporary folders, removed at the end of each test

test_that("check_output_folder() refuses a folder that is not empty, unless overwrite = TRUE", {
  root <- tempfile("wb_out_")
  dir.create(root)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  new_dir <- file.path(root, "new")
  expect_silent(check_output_folder(new_dir))
  expect_false(dir.exists(new_dir))
  empty_dir <- file.path(root, "empty")
  dir.create(empty_dir)
  expect_silent(check_output_folder(empty_dir))
  full_dir <- file.path(root, "full")
  dir.create(full_dir)
  writeLines("x", file.path(full_dir, "runs.csv"))
  expect_error(check_output_folder(full_dir), "exists and is not empty")
  expect_silent(check_output_folder(full_dir, overwrite = TRUE))
  hidden_dir <- file.path(root, "hidden")
  dir.create(hidden_dir)
  writeLines("x", file.path(hidden_dir, ".hidden"))
  expect_error(check_output_folder(hidden_dir), "exists and is not empty")
  expect_error(check_output_folder(file.path(full_dir, "runs.csv")), "is a file")
  expect_error(check_output_folder(""), "give one output folder")
  expect_error(check_output_folder(NA_character_), "give one output folder")
})

test_that("check_output_folder() refuses a folder inside the forbidden folder", {
  root <- tempfile("wb_repo_")
  dir.create(file.path(root, "wp2_injection", "output"), recursive = TRUE)
  on.exit(unlink(root, recursive = TRUE), add = TRUE)
  archived <- file.path(root, "wp2_injection", "output", "12_m1_crossed_design")
  expect_error(check_output_folder(archived, forbidden_root = root), "lies inside")
  expect_false(dir.exists(archived))
  expect_error(check_output_folder(root, forbidden_root = root), "lies inside")
  if (.Platform$OS.type == "windows") {
    expect_error(check_output_folder(toupper(archived), forbidden_root = root), "lies inside")
  }
  # a folder whose name starts with the name of the forbidden folder lies outside it
  expect_silent(check_output_folder(paste0(root, "_sibling"), forbidden_root = root))
  # relative paths, as the scripts give them from wp2_injection/
  old <- setwd(file.path(root, "wp2_injection"))
  on.exit(setwd(old), add = TRUE, after = FALSE)
  expect_error(check_output_folder("output/smoke", forbidden_root = ".."), "lies inside")
  expect_error(check_output_folder("../other/smoke", forbidden_root = ".."), "lies inside")
  expect_silent(check_output_folder(tempfile("wb_outside_"), forbidden_root = ".."))
})

test_that("a trial without outcomes has the columns of a completed trial, empty", {
  m1 <- model_m1()
  values <- sin(seq_len(60))
  ok <- run_trial(m1, draw_fixed(m1, input_fixed_series(values, 20L, 1e-3, value_unit = "rad")),
                  seed_of = function(a) 97000001 + a, n_transient = 20L, n_keep = 60L)
  inp_fall <- input_fixed_series(values, 20L, 0.05, value_unit = "rad")
  fall <- run_trial(m1, draw_fixed(m1, inp_fall), seed_of = function(a) 97000001 + a,
                    n_transient = 20L, n_keep = 60L)
  expect_false(ok$fell)
  expect_true(fall$fell)
  expect_identical(names(fall$summary), names(ok$summary))
  keys <- c("model", "injection", "cell", "replicate", "n_redraws", "fell", "n_falls",
            "n_numerical_failures")
  outcome <- setdiff(names(ok$summary), keys)
  expect_identical(outcome, outcome_columns(m1))
  expect_true(all(is.na(unlist(fall$summary[outcome]))))
  # estimate_all() of a run that fell gives the same columns, empty
  run <- simulate_run(m1, inp_fall, n_cycles = 80L)
  expect_true(run$fell)
  row <- estimate_all(run, m1)
  expect_identical(names(row), names(ok$summary)[seq_len(ncol(row))])
  expect_true(all(is.na(unlist(row[outcome]))))
})

test_that("write_campaign_log() records the generator, pkgload, parallel and the repository", {
  dir <- tempfile("wb_log_")
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  m1 <- model_m1()
  t0 <- Sys.time()
  path <- write_campaign_log(dir, campaign = "test", purpose = "test", script = "none",
                             producer = "test", master_seed = 97000000, design = list(n = 1),
                             model = m1, injection = m1$default_injection,
                             settings = m1$estimation_settings, start = t0, end = t0,
                             campaign_path = dir)
  log <- jsonlite::fromJSON(path)
  expect_identical(log$rng_kind, RNGkind())
  expect_true(all(c("pkgload", "parallel") %in% names(log$package_versions)))
  expect_identical(log$package_versions$parallel, as.character(utils::packageVersion("parallel")))
  expect_true(all(c("commit", "modified_files") %in% names(log$whole_repository)))
})
