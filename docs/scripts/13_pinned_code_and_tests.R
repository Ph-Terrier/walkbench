# Title: Worked example of the chapter "Pinned code, tests and reproducibility" of the
#   walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session of SIM-ACI (Claude Fable 5.2), without a code-generation
#   model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: path of the package (.. in place of ../../testbed); folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-13
# Description: reads the manifest of the pinned files of walkbench 0.6.2, checks the pinned
#   files against it, asks for the path of one pinned file, loads the engine of RACING v1.0.2
#   and compares the names of its functions with the exports of the package. The hidden blocks
#   write the signatures of the functions, count the test blocks of each test file of the
#   package from the source files (the test suite itself is not run here), register the
#   numbers quoted in the chapter, draw its figure and run the checks on which the text
#   relies: the sizes and the line endings of the pinned files, the MD5 sum of the engine, the
#   two copies of the engine, and the attachment of deSolve by the pinned walker file. The
#   script draws no random number and simulates no model, so it uses none of the seeds
#   reserved for the chapter (cells 130 to 139 of the block of the guide).
# Dependencies: walkbench 0.6.2 (loaded by the driver), digest, tools, utils, graphics,
#   grDevices
# Input: ../tests/testthat/test-*.R and ../README.md (read only)
# Output: generated/13_pinned_code_and_tests/: snippets/*.txt, output/*.txt,
#   signatures/*.txt, numbers.tex, numbers.csv, fig_checks.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/13_pinned_code_and_tests.R

## ---- hidden: setup ----
library(utils)
library(tools)
library(graphics)
library(grDevices)
for (f in c("pinned_file", "check_pinned_files", "load_racing")) guide_signature(f)
# state of the search path before any pinned file is evaluated
desolve_attached_at_start <- "package:deSolve" %in% search()

## ---- manifest ----
manifest_file <- system.file("pinned", "MANIFEST.csv", package = "walkbench")
manifest <- read.csv(manifest_file, colClasses = "character")
data.frame(file = basename(manifest$pinned_path), bytes_lf = manifest$bytes_lf,
           sha256_lf = paste0(substr(manifest$sha256_lf, 1, 16), "..."))
checked <- check_pinned_files()
data.frame(file = basename(checked$pinned_path), identical = checked$identical)

## ---- pinned-file ----
engine <- "simaci/feasibility/scripts/racing/rosenstein_divergence.R"
path <- pinned_file(engine)
basename(path)
tryCatch(pinned_file("not/in/the/manifest.R"), error = conditionMessage)

## ---- racing ----
racing <- load_racing()
sort(ls(racing))
identical(racing, load_racing())
intersect(ls(racing), getNamespaceExports("walkbench"))

## ---- hidden: checks-pinned ----
guide_number("n_pinned", nrow(manifest), "%d", "pinned files of the manifest")
guide_number("n_manifest_columns", ncol(manifest), "%d", "columns of the manifest")
guide_number("n_exports", length(getNamespaceExports("walkbench")), "%d", "exported functions of walkbench")
guide_number("n_racing", length(ls(racing)), "%d", "functions of the engine of RACING")
if (!all(checked$identical)) stop("a pinned file differs from the manifest")
# columns of the manifest and of the check, as the chapter names them
if (!identical(names(manifest), c("pinned_path", "origin_repository", "origin_path", "origin_commit",
                                  "git_blob", "sha256_lf", "bytes_lf", "license", "copied_on",
                                  "copied_by"))) stop("the columns of the manifest differ from the table of the chapter")
if (!identical(names(checked), c("pinned_path", "sha256_manifest", "sha256_file", "identical"))) {
  stop("the columns of check_pinned_files() differ from the text of the chapter")
}
if (!identical(sort(basename(manifest$pinned_path)),
               sort(c("00_functions_walker.R", "rosenstein_divergence.R", "LICENSE", "README.md",
                      "07b_walker_prediction.R")))) stop("the pinned files differ from the table of the chapter")
if (!all(vapply(ls(racing), function(f) is.function(racing[[f]]), logical(1)))) stop("the environment of the engine was expected to hold functions only")
if (!identical(environmentName(parent.env(racing)), "R_GlobalEnv")) stop("the parent of the environment of the engine was expected to be the global environment")
# the pinned files are stored with LF line endings: their sizes equal the column bytes_lf
pinned_root <- system.file("pinned", package = "walkbench")
sizes <- file.size(file.path(pinned_root, manifest$pinned_path))
if (!all(sizes == as.numeric(manifest$bytes_lf))) stop("the size of a pinned file differs from bytes_lf")
if (!all(nchar(manifest$sha256_lf) == 64L)) stop("a SHA-256 sum of the manifest has not 64 characters")
# a copy of the engine with CRLF line endings has the same sum under the rule of the package
# and another sum without the conversion
bytes <- readBin(path, "raw", file.size(path))
is_lf <- bytes == as.raw(10L)
position <- seq_along(bytes) + cumsum(is_lf)
crlf <- raw(length(bytes) + sum(is_lf))
crlf[position] <- bytes
crlf[position[is_lf] - 1L] <- as.raw(13L)
crlf_copy <- tempfile(fileext = ".R")
writeBin(crlf, crlf_copy)
sum_rule <- walkbench:::sha256_lf(crlf_copy)
sum_plain <- digest::digest(file = crlf_copy, algo = "sha256")
unlink(crlf_copy)
row_engine <- manifest$pinned_path == engine
if (!identical(sum_rule, manifest$sha256_lf[row_engine])) stop("the sum of a CRLF copy was expected to equal the manifest")
if (identical(sum_plain, manifest$sha256_lf[row_engine])) stop("the plain sum of a CRLF copy was expected to differ")
if (!identical(digest::digest(file = path, algo = "sha256"), manifest$sha256_lf[row_engine])) stop("the plain sum of the stored file was expected to equal the manifest")
# the MD5 sum that load_racing() checks, and the two sums given by the provenance file
md5_engine <- unname(tools::md5sum(bytes = charToRaw(paste(readLines(path, warn = FALSE), collapse = "\n"))))
if (!identical(md5_engine, walkbench:::racing_md5)) stop("the MD5 sum of the engine differs from the constant of the package")
provenance <- readLines(pinned_file("simaci/feasibility/scripts/racing/README.md"), warn = FALSE)
if (!any(grepl(md5_engine, provenance, fixed = TRUE)) ||
    !any(grepl(manifest$sha256_lf[row_engine], provenance, fixed = TRUE))) stop("the provenance file was expected to give the two sums of the engine")
# the pinned walker file attaches deSolve and holds its own copy of the engine
if (desolve_attached_at_start) stop("deSolve was expected not to be attached before the first model")
m1 <- model_m1()
if (!("package:deSolve" %in% search())) stop("deSolve was expected to be attached after model_m1()")
walker <- walkbench:::m1_walker_env()
if (identical(walker$racing_engine, racing)) stop("the walker file was expected to hold its own copy of the engine")
if (!setequal(ls(walker$racing_engine), ls(racing))) stop("the two copies of the engine were expected to hold the same functions")
used <- c("walker_rhs", "heelstrike_map", "integrate_step", "integrate_step_terrain",
          "to_continuous", "find_fixed_point", "normalize_strides", "arfima_series", "dfa_alpha")
if (!all(used %in% ls(walker))) stop("a function of the walker file named in the text is missing")
if (length(m1$injection_points) != 5L) stop("the text names five injection points of M1")

## ---- hidden: test-blocks ----
# test blocks of each test file, counted from the source: calls of test_that() at the top
# level, and those inside a loop at the top level once per element of the loop
source_dir <- ".."
test_files <- sort(list.files(file.path(source_dir, "tests", "testthat"), pattern = "^test-.*[.]R$"))
is_test <- function(e) is.call(e) && identical(e[[1]], as.name("test_that"))
count_blocks <- function(file) {
  n <- 0L
  for (e in parse(file, keep.source = FALSE)) {
    if (is_test(e)) n <- n + 1L
    if (is.call(e) && identical(e[[1]], as.name("for"))) {
      inside <- sum(vapply(as.list(e[[4]])[-1], is_test, logical(1)))
      n <- n + inside * length(eval(e[[3]]))
    }
  }
  n
}
blocks <- vapply(file.path(source_dir, "tests", "testthat", test_files), count_blocks, integer(1))
for (i in seq_along(test_files)) {
  key <- gsub("-", "_", sub("^test-(.*)[.]R$", "tests_\\1", test_files[i]))
  guide_number(key, blocks[i], "%d", paste("test blocks of", test_files[i]))
}
guide_number("tests_files", length(test_files), "%d", "test files of the package")
guide_number("tests_total", sum(blocks), "%d", "test blocks of the package")
readme <- paste(readLines(file.path(source_dir, "README.md"), warn = FALSE, encoding = "UTF-8"), collapse = " ")
if (!grepl(sprintf("runs the %d test blocks", sum(blocks)), readme, fixed = TRUE)) stop("the README of the package states another number of test blocks")
if (!grepl("this comparison is skipped", readme, fixed = TRUE)) stop("the README was expected to describe the comparison that is skipped")
skips <- vapply(file.path(source_dir, "tests", "testthat", test_files),
                function(f) sum(grepl("skip_if_not(", readLines(f, warn = FALSE), fixed = TRUE)), integer(1))
if (sum(skips) != 1L || names(skips)[skips == 1L] != file.path(source_dir, "tests", "testthat", "test-estimators.R")) {
  stop("one conditional skip was expected, in test-estimators.R")
}
# versions that the driver records in session.txt
for (p in c("deSolve", "RANN", "MASS", "digest", "jsonlite", "pkgload")) {
  guide_number(paste0("v_", p), as.character(utils::packageVersion(p)), "%s", paste("version of the package", p))
}

## ---- hidden: figure-checks ----
col <- guide_colors()
guide_figure("checks", width_cm = 15.5, height_cm = 5.4, draw = function() {
  par(mar = c(0.1, 0.1, 0.1, 0.1))
  plot(NA, xlim = c(0, 100), ylim = c(0, 33), axes = FALSE, xlab = "", ylab = "", xaxs = "i",
       yaxs = "i")
  x0 <- c(0.5, 36, 71.5)
  width <- 28
  top <- c(27.5, 17.5, 7.5)
  box <- function(i, j, txt, check = FALSE) {
    rect(x0[j], top[i] - 7.5, x0[j] + width, top[i], lwd = 0.9,
         col = if (check) col[["light"]] else "white",
         border = if (check) col[["blue"]] else col[["gray"]])
    text(x0[j] + width / 2, top[i] - 3.75, txt, cex = 0.9)
  }
  link <- function(i, j) {
    arrows(x0[j] + width + 0.6, top[i] - 3.75, x0[j + 1] - 0.6, top[i] - 3.75, length = 0.06,
           col = col[["gray"]], lwd = 0.9)
  }
  text(x0 + width / 2, 31.2, c("Call", "Check", "After the check"), font = 2, cex = 0.9)
  box(1, 1, "model_m1() or input_arfima(),\nfirst call")
  box(1, 2, "SHA-256 sums of the walker\nfile and of the engine file", check = TRUE)
  box(1, 3, "walker file evaluated in\na private environment")
  box(2, 1, "estimate_divergence(),\nfirst call of load_racing()")
  box(2, 2, "SHA-256 sum, then MD5 sum,\nof the engine file", check = TRUE)
  box(2, 3, "engine file evaluated in\na private environment")
  box(3, 1, "write_campaign_log(),\ncheck_pinned_files()")
  box(3, 2, "SHA-256 sums of the\nfive pinned files", check = TRUE)
  box(3, 3, "field pinned_files_identical\nof the run log")
  for (i in 1:3) for (j in 1:2) link(i, j)
})
if (nrow(manifest) != 5L) stop("the figure names five pinned files")

## ---- hidden: check-outputs ----
# no printed output shows a path of this computer, its host name or a user name, and no
# printed line exceeds 80 characters
printed <- unlist(lapply(list.files(file.path(guide_dir(), c("output", "snippets")),
                                    full.names = TRUE), readLines, warn = FALSE))
private <- c(Sys.info()[["nodename"]], Sys.info()[["user"]],
             substr(Sys.info()[["user"]], 1, 6), "Users", "AppData", "OneDrive", "Rtmp")
if (any(vapply(private, function(w) any(grepl(tolower(w), tolower(printed), fixed = TRUE)),
               logical(1)))) stop("a printed block shows a path, a host name or a user name")
if (any(nchar(printed) > 80L)) stop("a printed line has more than 80 characters")
