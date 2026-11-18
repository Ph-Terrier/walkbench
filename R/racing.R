# Title: Access to the pinned Rosenstein engine of RACING v1.0.2
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: loads the pinned, unmodified file rosenstein_divergence.R of RACING v1.0.2
#   into a private environment, after the SHA-256 check of the manifest and the MD5 check
#   of the RACING README (line-ending independent). Stops when a check fails
#   (specification, section 9.1). The functions of RACING are not exported.
# Dependencies: tools, RANN (used by the RACING neighbor search)
# Input: inst/pinned/simaci/feasibility/scripts/racing/rosenstein_divergence.R
# Output: none

racing_pinned_path <- "simaci/feasibility/scripts/racing/rosenstein_divergence.R"
racing_md5 <- "29940702bbd3f155023274cf62e8cc2f"

#' Load the pinned RACING engine
#'
#' @return An environment that holds the functions of `rosenstein_divergence.R` (RACING
#'   v1.0.2): `embed_time_series()`, `find_nearest_neighbors()`, `compute_divergence()`,
#'   `estimate_mean_period()` and `rosenstein_divergence()`. The environment is cached.
#' @export
load_racing <- function() {
  if (!is.null(.wb$racing)) return(.wb$racing)
  if (!requireNamespace("RANN", quietly = TRUE)) {
    stop("package 'RANN' is required by the RACING neighbor search")
  }
  path <- pinned_file(racing_pinned_path)
  md5 <- unname(tools::md5sum(bytes = charToRaw(
    paste(readLines(path, warn = FALSE), collapse = "\n"))))
  if (!identical(md5, racing_md5)) {
    stop("RACING engine differs from release v1.0.2: ", path)
  }
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env, keep.source = FALSE)
  .wb$racing <- env
  env
}
