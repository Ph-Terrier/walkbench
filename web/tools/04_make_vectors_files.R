# Title: Test vectors of the reading of a series from a file by the web simulator
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written on 2026-11-21 by the orchestrating model of the web-simulator session of
#   SIM-ACI (Claude Opus 5.5), without a code-generation model, after the decision of the author
#   of 2026-11-21 relayed by the coordination session of SIM-ACI (Claude Fable 5.2)
# Date: 2026-11-21
# Description: writes test files (valid series, decimal comma, semicolon, several numbers on a
#   line, two columns, header line, comment lines, NA, Inf, hexadecimal, quoted number,
#   no-break space, thousands separator, empty file, byte-order mark, CR line ends) and reads
#   each one with scan(file, comment.char = "#"), the call of the R code exported by the page.
#   For each file, the vectors hold the bytes of the file, the outcome that the reading rule of
#   the page must give (accepted, or refused with the numbers and kinds of the reported lines),
#   the outcome of scan(), the IEEE bits of the numbers that scan() read and, for a series of
#   finite numbers, the output of input_fixed_series() of walkbench. The checks are in
#   tests/run_tests_files.js. The script reads the package and writes no file inside it.
# Dependencies: pkgload, jsonlite, digest; walkbench and its imports
# Input: the source of walkbench (folder testbed/ of the SIM-ACI repository); the optional
#   environment variable WALKBENCH_GIT gives the git executable for the record of the commit
# Output: web/tests/vectors/file_vectors.json
# Usage: Rscript tools/04_make_vectors_files.R (paths resolved from the location of the script)

library(pkgload)
library(jsonlite)
library(digest)

# --- Paths, from the location of this script ---------------------------------------------
args_all <- commandArgs(trailingOnly = FALSE)
file_arg <- sub("^--file=", "", grep("^--file=", args_all, value = TRUE))
if (length(file_arg) != 1L) stop("run this script with Rscript")
script_dir <- dirname(normalizePath(file_arg, winslash = "/"))
web_dir <- dirname(script_dir)
pkg_dir <- dirname(web_dir)
out_file <- file.path(web_dir, "tests", "vectors", "file_vectors.json")

pkgload::load_all(pkg_dir, quiet = TRUE)
t_start <- Sys.time()

hashed <- c("DESCRIPTION", "R/perturbation_port.R")
sha256 <- vapply(hashed, function(f) digest::digest(file = file.path(pkg_dir, f), algo = "sha256"),
                 character(1))
git <- git_state(pkg_dir)
vers <- function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(e) NA_character_)

# --- Test files ------------------------------------------------------------------------------
lf <- function(...) charToRaw(paste0(paste(c(...), collapse = "\n"), "\n"))
set.seed(11)
x <- rnorm(600) * 0.01
valid_lines <- c("# synthetic series of 600 values", "",
                 sprintf("%.17g", x[1:200]), sprintf("%.6f", x[201:400]),
                 paste(sprintf("%.3e", x[401:599]), c("# trailing comment", rep("", 198))),
                 sprintf("%.15g", x[600]))
valid_lines <- sub(" +$", "", valid_lines)
y <- rnorm(150)
z <- rnorm(10000) * 10^sample(-8:8, 10000, replace = TRUE)
conv_lines <- c(sprintf("%.17g", z[1:2500]), sprintf("%.15g", z[2501:5000]),
                sprintf("%.6f", z[5001:7500]), sprintf("%.20e", z[7501:10000]))

# page: TRUE when the reading rule of the page accepts the file; bad: lines reported by the page
cases <- list(
  list(name = "valid: 600 values, comment line, blank line, trailing comment, LF",
       bytes = lf(valid_lines), page = TRUE),
  list(name = "valid: 150 values, byte-order mark and CRLF",
       bytes = c(as.raw(c(0xEF, 0xBB, 0xBF)),
                 charToRaw(paste0(paste(sprintf("%.15g", y), collapse = "\r\n"), "\r\n"))),
       page = TRUE),
  list(name = "valid: forms of numbers (.5, +.5, 5., exponents, leading zeros, -0), CR line ends",
       bytes = charToRaw(paste0(paste(c(".5", "+.5", "5.", "-1.0e+2", "1E-3", "007", "-0", "  0.25\t",
                                        "\t-3.5  # comment", "12"), collapse = "\r"), "\r")),
       page = TRUE),
  list(name = "valid: comment lines only", bytes = lf("# a", "", "# b"), page = TRUE),
  list(name = "valid: empty file", bytes = raw(0), page = TRUE),
  list(name = "conversion: 10,000 values with 6 to 21 significant digits",
       bytes = lf(conv_lines), page = TRUE),
  list(name = "decimal comma", bytes = lf("0.25", "0.5", "0,123", "1.5", "-1,5e-2", "2"),
       page = FALSE, bad = list(c(3, "comma"), c(5, "comma"))),
  list(name = "semicolon between two numbers", bytes = lf("0.1", "0.1;0.2", "0.3"),
       page = FALSE, bad = list(c(2, "format"))),
  list(name = "several numbers on a line", bytes = lf("0.1 0.2", "0.3"),
       page = FALSE, bad = list(c(1, "format"))),
  list(name = "two columns separated by a tab", bytes = lf("1\t0.25", "2\t0.5", "3\t0.125"),
       page = FALSE, bad = list(c(1, "format"), c(2, "format"), c(3, "format"))),
  list(name = "header line", bytes = lf("value", "0.5", "0.25"),
       page = FALSE, bad = list(c(1, "format"))),
  list(name = "NA", bytes = lf("0.5", "NA", "0.25"), page = FALSE, bad = list(c(2, "format"))),
  list(name = "Inf and an overflow", bytes = lf("Inf", "1e400"),
       page = FALSE, bad = list(c(1, "format"), c(2, "format"))),
  list(name = "hexadecimal number", bytes = lf("0.5", "0x1A"), page = FALSE, bad = list(c(2, "format"))),
  list(name = "quoted number", bytes = lf("\"0.5\""), page = FALSE, bad = list(c(1, "format"))),
  list(name = "no-break space after a number", bytes = c(charToRaw("0.5"), as.raw(c(0xC2, 0xA0)), charToRaw("\n")),
       page = FALSE, bad = list(c(1, "format"))),
  list(name = "thousands separator", bytes = lf("1,234.5"), page = FALSE, bad = list(c(1, "format")))
)

hex_bits <- function(v) {
  if (!length(v)) return(character(0))
  m <- matrix(as.character(writeBin(as.numeric(v), raw(), endian = "big")), nrow = 8L)
  apply(m, 2L, paste, collapse = "")
}

sd_test <- 0.00234
out_cases <- lapply(cases, function(cs) {
  f <- tempfile(fileext = ".txt")
  writeBin(cs$bytes, f)
  v <- tryCatch(scan(f, comment.char = "#", quiet = TRUE), error = function(e) e)
  unlink(f)
  r_ok <- !inherits(v, "error")
  rec <- list(name = cs$name, bytes_base64 = jsonlite::base64_enc(cs$bytes),
              page_accepts = cs$page,
              page_bad = lapply(if (is.null(cs$bad)) list() else cs$bad,
                                function(b) list(line = as.integer(b[1]), kind = b[2])),
              r_accepts = r_ok,
              r_error = if (r_ok) NA_character_ else conditionMessage(v))
  if (r_ok) {
    rec$r_n <- length(v)
    rec$r_bits <- I(hex_bits(v))
    rec$r_all_finite <- all(is.finite(v))
    if (length(v) >= 3L && all(is.finite(v))) {
      rec$fixed_series <- I(input_fixed_series(v, n_transient = 0L, sd = sd_test)$values)
    }
  }
  rec
})

meta <- list(
  produced_by = paste("tools/04_make_vectors_files.R, written by the orchestrating model of the",
                      "web-simulator session of SIM-ACI (Claude Opus 5.5)"),
  walkbench_version = vers("walkbench"),
  loaded_from = "source tree, pkgload::load_all()",
  git_commit = git$commit, git_modified_files_in_package = git$modified,
  sha256_of_working_tree_files = as.list(sha256),
  r_version = R.version.string, platform = R.version$platform,
  locale_numeric = Sys.getlocale("LC_NUMERIC"),
  scan_call = "scan(file, comment.char = \"#\")",
  sd_of_fixed_series = sd_test,
  date = format(t_start, "%Y-%m-%dT%H:%M:%S%z"))

jsonlite::write_json(list(meta = meta, cases = out_cases), out_file, auto_unbox = TRUE,
                     digits = I(17), na = "null")
cat(sprintf("written: %s (%.1f MB), %d files\n", out_file, file.size(out_file) / 1e6, length(cases)))
for (cs in out_cases) {
  cat(sprintf("%-80s R: %s\n", cs$name,
              if (cs$r_accepts) paste(cs$r_n, "numbers") else paste("error:", cs$r_error)))
}
