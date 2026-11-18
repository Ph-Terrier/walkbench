# Title: Tests of the pinned code (manifest, SHA-256, RACING release, exports)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: specification, section 13.3, test-pinned-code.R
# Dependencies: testthat, walkbench
# Input: inst/pinned/
# Output: none

test_that("every pinned file matches the manifest", {
  chk <- check_pinned_files()
  expect_gt(nrow(chk), 0)
  expect_true(all(chk$identical))
})

test_that("the pinned RACING file is release v1.0.2", {
  path <- pinned_file("simaci/feasibility/scripts/racing/rosenstein_divergence.R")
  # SHA-256 and MD5 given in feasibility/scripts/racing/README.md
  expect_identical(walkbench:::sha256_lf(path),
                   "37c0441c38119ae22dd19c849f06c91f6a0b2169063fb44c2370786639d367aa")
  md5 <- unname(tools::md5sum(bytes = charToRaw(
    paste(readLines(path, warn = FALSE), collapse = "\n"))))
  expect_identical(md5, "29940702bbd3f155023274cf62e8cc2f")
  r <- load_racing()
  expect_true(all(vapply(c("find_nearest_neighbors", "compute_divergence",
                           "embed_time_series"), function(f) is.function(r[[f]]), TRUE)))
})

test_that("the package exports no function of RACING", {
  ex <- getNamespaceExports("walkbench")
  expect_false(any(c("find_nearest_neighbors", "compute_divergence", "embed_time_series",
                     "estimate_mean_period", "rosenstein_divergence") %in% ex))
})

test_that("a pinned path absent from the manifest is refused", {
  expect_error(pinned_file("not/in/the/manifest.R"), "absent from the manifest")
})
