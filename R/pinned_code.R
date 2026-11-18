# Title: Pinned code: manifest, SHA-256 checks and access to the pinned files
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: the package copies code taken from other repositories, unchanged, into
#   inst/pinned/ (specification, section 9.1). The manifest inst/pinned/MANIFEST.csv gives
#   for each file its origin, git blob and SHA-256 sum with LF line endings. A pinned file
#   is used only after its sum has been recomputed and found equal to the manifest.
# Dependencies: digest, utils
# Input: inst/pinned/MANIFEST.csv and the pinned files
# Output: none

pinned_dir <- function() {
  d <- system.file("pinned", package = "walkbench")
  if (!nzchar(d)) stop("folder inst/pinned not found in the package walkbench")
  d
}

read_manifest <- function() {
  utils::read.csv(file.path(pinned_dir(), "MANIFEST.csv"), colClasses = "character",
                  stringsAsFactors = FALSE)
}

# SHA-256 of a file after conversion of CRLF line endings to LF
sha256_lf <- function(path) {
  bytes <- readBin(path, what = "raw", n = file.info(path)$size)
  cr <- which(bytes == as.raw(13L))
  if (length(cr) > 0L) {
    nxt <- c(bytes[-1L], as.raw(0L))
    drop <- cr[nxt[cr] == as.raw(10L)]
    if (length(drop) > 0L) bytes <- bytes[-drop]
  }
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}

#' Path of a pinned file, after its SHA-256 check
#'
#' @param pinned_path path of the file relative to `inst/pinned/`, as in the manifest.
#' @return The normalized path of the file. The function stops when the file is missing,
#'   absent from the manifest, or when its SHA-256 sum (LF line endings) differs from the
#'   manifest.
#' @export
pinned_file <- function(pinned_path) {
  man <- read_manifest()
  row <- man[man$pinned_path == pinned_path, , drop = FALSE]
  if (nrow(row) != 1L) stop("file absent from the manifest: ", pinned_path)
  path <- file.path(pinned_dir(), pinned_path)
  if (!file.exists(path)) stop("pinned file missing: ", path)
  sha <- sha256_lf(path)
  if (!identical(sha, row$sha256_lf)) {
    stop(sprintf("pinned file differs from the manifest: %s (SHA-256 %s, manifest %s)",
                 pinned_path, sha, row$sha256_lf))
  }
  normalizePath(path, winslash = "/")
}

#' Check every pinned file against the manifest
#'
#' @return A data frame with one row per pinned file: path, SHA-256 of the manifest,
#'   SHA-256 of the file (LF line endings) and a logical column `identical`.
#' @export
check_pinned_files <- function() {
  man <- read_manifest()
  sha <- vapply(file.path(pinned_dir(), man$pinned_path), sha256_lf, character(1))
  data.frame(pinned_path = man$pinned_path, sha256_manifest = man$sha256_lf,
             sha256_file = unname(sha), identical = unname(sha) == man$sha256_lf,
             stringsAsFactors = FALSE)
}
