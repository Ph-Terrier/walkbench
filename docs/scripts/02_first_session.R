# Title: Example of the chapter "Installation and first session" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: path of the package (.. in place of ../../testbed), name of its temporary copy; folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: checks the facts on installation that the chapter states, then runs a first
#   session. The hidden blocks read the DESCRIPTION file of walkbench 0.7.0, install a
#   temporary copy of the source of the package into a temporary library and call the
#   installed package from a new R process; the copy and the library are removed at the end,
#   and nothing is written into the folder of the package. The printed blocks build model M1,
#   define one experiment as a function of a model (uniform uncorrelated input, run, removal of
#   the transient cycles, outcomes) and apply it to M1 and to M2. Seeds: 90,002,001 and
#   90,002,002 (cell 20 of the block of the guide).
# Dependencies: walkbench 0.7.0 (loaded by the driver), utils
# Input: DESCRIPTION of the package (read only)
# Output: generated/02_first_session/: snippets/*.txt, output/*.txt, numbers.tex, numbers.csv,
#   run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/02_first_session.R

## ---- hidden: description ----
library(utils)
package_dir <- ".."
desc <- read.dcf(file.path(package_dir, "DESCRIPTION"))[1, ]
imports <- trimws(strsplit(gsub("\n", " ", desc[["Imports"]]), ",")[[1]])
base_packages <- rownames(installed.packages(priority = "base"))
contributed <- sort(setdiff(imports, base_packages), method = "radix")
# the chapter names these facts in its text: stop when the package says otherwise
if (!identical(contributed, c("MASS", "RANN", "deSolve", "digest", "jsonlite"))) {
  stop("the imports of the package changed: ", paste(contributed, collapse = ", "))
}
if (!identical(desc[["URL"]], "https://github.com/Ph-Terrier/walkbench")) stop("the URL changed")
if (!identical(desc[["BugReports"]], "https://github.com/Ph-Terrier/walkbench/issues")) {
  stop("the address of the issues changed")
}
if (!identical(desc[["License"]], "MIT + file LICENSE")) stop("the license changed")
if (length(list.files(file.path(package_dir, "src"))) > 0) stop("the package has compiled code")
# version of R that the code needs: the package and its pinned walker file call
# tools::md5sum(bytes = ), an argument that the NEWS file of R lists among the new features of
# R 4.5.0; the script stops when DESCRIPTION declares another version
uses_bytes <- vapply(c(file.path(package_dir, "R", "racing.R"),
                       file.path(package_dir, "inst", "pinned", "simaci", "feasibility",
                                 "scripts", "00_functions_walker.R")),
                     function(f) any(grepl("md5sum(bytes =", readLines(f, warn = FALSE), fixed = TRUE)),
                     logical(1))
if (!all(uses_bytes)) stop("the package no longer calls tools::md5sum(bytes = )")
news <- readLines(file.path(R.home("doc"), "NEWS"), warn = FALSE)
heads <- grep("^CHANGES IN R ", news)
start <- heads[news[heads] == "CHANGES IN R 4.5.0:"]
if (length(start) != 1L) stop("the NEWS file of R has no section on R 4.5.0")
end <- min(heads[heads > start])
if (!any(grepl("bytes= argument", news[start:end], fixed = TRUE))) {
  stop("the NEWS file of R does not list the argument bytes of md5sum() under R 4.5.0")
}
r_declared <- sub("^.*R \\(>= ([0-9.]+)\\).*$", "\\1", desc[["Depends"]])
if (!identical(r_declared, "4.5.0")) {
  stop("DESCRIPTION declares R ", r_declared, "; the text of the chapter states 4.5.0")
}
guide_number("r_declared", r_declared, "%s",
             "version of R that DESCRIPTION declares")
guide_number("r_needed", "4.5.0", "%s",
             "version of R that introduced the argument bytes of tools::md5sum(), read in the NEWS file of R")
guide_number("version", desc[["Version"]], "%s", "version of the package in DESCRIPTION")
guide_number("n_imports", length(contributed), "%d", "contributed packages that walkbench imports")

## ---- hidden: installation ----
# installation of a temporary copy of the source into a temporary library, then a call of the
# installed package from a new R process
tmp <- tempfile("walkbench_install_")
dir.create(tmp)
lib <- file.path(tmp, "library")
dir.create(lib)
# the copy needs a folder name other than walkbench: under Windows, install.packages()
# does not install a package whose name matches a loaded namespace
src <- file.path(tmp, "source")
dir.create(src)
file.copy(list.files(package_dir, all.files = TRUE, full.names = TRUE, no.. = TRUE), src,
          recursive = TRUE)
install.packages(src, lib = lib, repos = NULL, type = "source", quiet = TRUE)
installed <- packageDescription("walkbench", lib.loc = lib)
if (!identical(installed$Version, desc[["Version"]])) stop("the installation failed")
child <- file.path(tmp, "check_installed.R")
writeLines(c(sprintf("library(walkbench, lib.loc = \"%s\")", gsub("\\\\", "/", lib)),
             "m1 <- model_m1()",
             "cat(transient_cycles(m1), all(check_pinned_files()$identical), \"\\n\")"),
           child)
out <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), shQuote(child),
                                stdout = TRUE, stderr = FALSE))
unlink(tmp, recursive = TRUE)
if (!identical(trimws(out[length(out)]), "20 TRUE")) {
  stop("the installed package did not run as expected: ", paste(out, collapse = " | "))
}
guide_number("install_checked", 1, "%d", "1 when the installation from a source copy and a call of the installed package succeeded")

## ---- model ----
m1 <- model_m1()
m1$label
m1$cycle_unit
round(m1$fixed_point()$period, 4)
m1$fall_rule

## ---- experiment ----
experiment <- function(model, rho, seed, n_keep = 300L) {
  n_transient <- transient_cycles(model)
  n_total <- n_transient + n_keep
  u <- input_iid_uniform(n_total, rho = rho, seed = seed)
  run <- simulate_run(model, u, n_cycles = n_total)
  estimate_all(trim_run(run, n_transient), model, input = u)
}
row_m1 <- experiment(m1, rho = 0.004, seed = 90002001)
dim(row_m1)
row_m1[, c("model", "injection", "mean_duration", "cv_duration")]
row_m1[, c("lambda_s", "lambda_l", "n_pairs", "alpha_duration")]

## ---- hidden: numbers-m1 ----
guide_number("n_keep", 300, "%d", "kept cycles of the first session")
guide_number("n_transient_m1", transient_cycles(m1), "%d", "transient cycles of M1")
guide_number("rho_m1", 4, "%d", "rho of the input of M1, in 1e-3 rad")
guide_number("n_columns", ncol(row_m1), "%d", "columns of the summary row of M1")
guide_number("t_star_m1", m1$fixed_point()$period, "%.4f", "step duration of the limit cycle of M1, in model time units")

## ---- second-model ----
m2 <- model_m2()
m2$id
m2$injection_points$floor_height$value_unit
row_m2 <- experiment(m2, rho = 0.0005, seed = 90002002)
row_m2[, c("model", "injection", "mean_duration", "cv_duration")]
row_m2[, c("lambda_s", "lambda_l", "n_pairs", "alpha_duration")]

## ---- hidden: numbers-m2 ----
guide_number("rho_m2", 0.5, "%.1f", "rho of the input of M2, in 1e-3 leg lengths")
guide_number("n_transient_m2", transient_cycles(m2), "%d", "transient cycles of M2 with its default arguments")
unit_m1 <- attr(row_m1, "divergence")$unit
unit_m2 <- attr(row_m2, "divergence")$unit
if (!identical(unit_m1, "per unit of model time") || !identical(unit_m2, "per stride")) {
  stop("the native units of the exponents changed: ", unit_m1, "; ", unit_m2)
}
