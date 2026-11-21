# Title: Driver of the example scripts of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: path of the package (.. in place of ../../testbed), folder of the driver, header and one line of session.txt; folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: runs one example script of the guide block by block and writes what the guide
#   prints. An example script is cut into blocks by comment lines of the form
#   "## ---- label ----". The code of each block is evaluated in one shared environment, in the
#   order of the script, and its console output is captured as R prints it at top level. For a
#   block whose label does not start with "hidden:", the driver writes the code to
#   snippets/<label>.txt and the output to output/<label>.txt; the guide prints both files
#   verbatim, so the code in the guide is the code that ran. Hidden blocks load the package,
#   register the numbers quoted in the text and draw the figures. The driver also writes the
#   run times of the blocks and the versions of R and of the packages (run_log.csv,
#   session.txt). The messages of R are set to English. The package walkbench is loaded from
#   the root folder of the repository with pkgload::load_all(), which does not write
#   into that folder.
# Dependencies: pkgload, utils; walkbench 0.7.0 in .. (deSolve, digest, jsonlite,
#   MASS, RANN)
# Input: scripts/00_guide_functions.R and the example script given as argument
# Output: generated/<chapter>/: snippets/*.txt, output/*.txt, signatures/*.txt, numbers.tex,
#   numbers.csv, fig_*.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/06_perturbation_port.R

library(pkgload)
library(utils)

Sys.setenv(LANGUAGE = "en")
args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) stop("give one example script, for example scripts/06_perturbation_port.R")
script <- args[1]
if (!file.exists(file.path("scripts", "00_guide_functions.R"))) {
  stop("run this driver from docs/")
}
if (!file.exists(script)) stop("script not found: ", script)
package_path <- ".."
if (!file.exists(file.path(package_path, "DESCRIPTION"))) stop("package folder not found: ", package_path)

source(file.path("scripts", "00_guide_functions.R"))
.guide$script <- basename(script)
.guide$chapter <- sub("\\.R$", "", basename(script))
out_dir <- guide_dir()
unlink(file.path(out_dir, c("snippets", "output", "signatures")), recursive = TRUE)
unlink(list.files(out_dir, pattern = "^fig_.*\\.pdf$", full.names = TRUE))
dir.create(file.path(out_dir, "snippets"), showWarnings = FALSE)
dir.create(file.path(out_dir, "output"), showWarnings = FALSE)

options(width = 80, warn = 1)
start <- Sys.time()
suppressMessages(load_all(package_path, quiet = TRUE))
if (utils::packageVersion("walkbench") != "0.7.0") {
  stop("the guide describes walkbench 0.7.0; the folder holds version ",
       as.character(utils::packageVersion("walkbench")))
}

# blocks of the script
lines <- readLines(script, warn = FALSE, encoding = "UTF-8")
is_label <- grepl("^## ---- .* ----\\s*$", lines)
if (!any(is_label)) stop("the script has no block label")
starts <- which(is_label)
ends <- c(starts[-1] - 1L, length(lines))
labels <- trimws(sub("^## ---- (.*) ----\\s*$", "\\1", lines[starts]))
if (anyDuplicated(labels)) stop("duplicated block label: ", labels[duplicated(labels)][1])

# evaluate one block; visible values are printed, as at top level
run_block <- function(code, envir) {
  exprs <- parse(text = code, keep.source = FALSE)
  utils::capture.output({
    for (e in exprs) {
      res <- withVisible(eval(e, envir))
      if (res$visible) print(res$value)
    }
  })
}

# remove the empty lines at both ends of a block
trim_block <- function(x) {
  keep <- which(nzchar(trimws(x)))
  if (length(keep) == 0L) character(0) else x[min(keep):max(keep)]
}

env <- new.env(parent = globalenv())
log <- data.frame(block = labels, shown = !grepl("^hidden:", labels), seconds = NA_real_,
                  stringsAsFactors = FALSE)
for (i in seq_along(labels)) {
  code <- trim_block(lines[(starts[i] + 1L):ends[i]])
  t0 <- Sys.time()
  out <- run_block(code, env)
  log$seconds[i] <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (log$shown[i]) {
    if (!grepl("^[a-z0-9-]+$", labels[i])) stop("invalid block label: ", labels[i])
    writeLines(code, file.path(out_dir, "snippets", paste0(labels[i], ".txt")), useBytes = TRUE)
    writeLines(out, file.path(out_dir, "output", paste0(labels[i], ".txt")), useBytes = TRUE)
  }
}
total <- as.numeric(difftime(Sys.time(), start, units = "secs"))

# run time of the shown blocks and of the whole script, for the text of the guide
guide_number("runtime_shown", sum(log$seconds[log$shown]), "%.0f",
             "run time of the blocks printed in the guide, in seconds")
guide_number("runtime_total", total, "%.0f",
             "run time of the whole script, package loading and figures included, in seconds")
guide_number("r_version", paste(R.version$major, R.version$minor, sep = "."), "%s",
             "version of R that ran the script")
guide_number("os", sub(" x64.*$", "", utils::sessionInfo()$running), "%s",
             "operating system of the computer that ran the script")
n_numbers <- guide_write_numbers()

utils::write.csv(rbind(log, data.frame(block = "TOTAL (with package loading)", shown = NA,
                                       seconds = total)),
                 file.path(out_dir, "run_log.csv"), row.names = FALSE)
vers <- function(p) as.character(utils::packageVersion(p))
writeLines(c(paste("Produced by: scripts/00_run_example.R (written by the guide session of SIM-ACI,",
                   "Claude Fable 5.2), which ran the example script named below and wrote every",
                   "file of this folder; the header of the example script names its producer"),
             sprintf("script: scripts/%s", basename(script)),
             sprintf("run on: %s", format(start, "%Y-%m-%d %H:%M:%S %z")),
             sprintf("total run time: %.1f s", total),
             R.version.string,
             sprintf("platform: %s", R.version$platform),
             sprintf("RNG kinds: %s", paste(RNGkind(), collapse = ", ")),
             sprintf("walkbench %s, loaded with pkgload::load_all() from the root folder of the repository",
                     vers("walkbench")),
             sprintf("deSolve %s, RANN %s, MASS %s, digest %s, jsonlite %s, pkgload %s",
                     vers("deSolve"), vers("RANN"), vers("MASS"), vers("digest"),
                     vers("jsonlite"), vers("pkgload")),
             sprintf("pinned files identical to the manifest: %s",
                     all(check_pinned_files()$identical))),
           file.path(out_dir, "session.txt"), useBytes = TRUE)
cat(sprintf("%s: %d blocks, %d numbers, %.1f s\n", basename(script), length(labels),
            n_numbers, total))
