# Title: Table of the appendix "Example scripts" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: reads the run logs that the driver wrote for the example scripts of the guide
#   (generated/<chapter>/run_log.csv and session.txt) and writes the table of the scripts:
#   chapter or appendix, script, number of blocks printed in the guide, number of numbers
#   registered, run time. It also registers the total run time and the versions of R and of
#   the packages, which must be the same for every script. The script runs after all the
#   others: build.py runs the scripts in the order of their names, and this name sorts last.
#   It prints no code block: every block is hidden.
# Dependencies: walkbench 0.7.0 (loaded by the driver), utils
# Input: generated/*/run_log.csv, generated/*/session.txt, generated/*/numbers.csv,
#   scripts/*.R
# Output: generated/D_example_scripts/: table_scripts.tex, numbers.tex, numbers.csv,
#   run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/D_example_scripts.R

## ---- hidden: table ----
library(utils)
scripts <- sort(list.files("scripts", pattern = "^([0-9]{2}|[A-Z])_.*[.]R$"), method = "radix")
scripts <- setdiff(scripts, c("00_guide_functions.R", "00_run_example.R", "D_example_scripts.R"))
info <- lapply(scripts, function(s) {
  d <- file.path("generated", sub("[.]R$", "", s))
  if (!file.exists(file.path(d, "run_log.csv"))) stop("script not run yet: ", s)
  log <- read.csv(file.path(d, "run_log.csv"), stringsAsFactors = FALSE)
  ses <- readLines(file.path(d, "session.txt"), warn = FALSE)
  num <- read.csv(file.path(d, "numbers.csv"), stringsAsFactors = FALSE)
  total <- log$seconds[grepl("^TOTAL", log$block)]
  list(script = s, shown = sum(log$shown, na.rm = TRUE), numbers = nrow(num), seconds = total,
       r = grep("^R version", ses, value = TRUE),
       packages = grep("^(walkbench|deSolve)", ses, value = TRUE),
       day = sub("^run on: ([0-9-]+).*$", "\\1", grep("^run on:", ses, value = TRUE)))
})
versions <- unique(lapply(info, function(i) c(i$r, i$packages)))
if (length(versions) != 1L) stop("the example scripts ran with different versions of R or of the packages")
prefix <- sub("_.*$", "", scripts)
where <- ifelse(grepl("^[0-9]", prefix), sprintf("Chapter %d", suppressWarnings(as.integer(prefix))),
                sprintf("Appendix %s", prefix))
tex_escape <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)
rows <- sprintf("%s & \\code{%s} & %d & %d & %.0f \\\\", where, scripts,
                vapply(info, function(i) i$shown, numeric(1)),
                vapply(info, function(i) i$numbers, numeric(1)),
                vapply(info, function(i) i$seconds, numeric(1)))
writeLines(c("% written by scripts/D_example_scripts.R; do not edit",
             "\\begin{tabularx}{\\textwidth}{@{}P{2.2cm}LP{1.6cm}P{1.6cm}P{1.9cm}@{}}",
             "\\toprule",
             "Part of the guide & Script & Printed blocks & Numbers & Run time (s) \\\\",
             "\\midrule", rows, "\\bottomrule", "\\end{tabularx}"),
           file.path(guide_dir(), "table_scripts.tex"), useBytes = TRUE)
guide_number("n_scripts", length(scripts), "%d", "example scripts listed in the appendix")
guide_number("total_seconds", sum(vapply(info, function(i) i$seconds, numeric(1))), "%.0f",
             "sum of the run times of the listed scripts, in seconds")
guide_number("total_minutes", sum(vapply(info, function(i) i$seconds, numeric(1))) / 60, "%.1f",
             "sum of the run times of the listed scripts, in minutes")
guide_number("max_seconds", max(vapply(info, function(i) i$seconds, numeric(1))), "%.0f",
             "longest run time of a listed script, in seconds")
guide_number("r_string", sub("^R version ", "", info[[1]]$r), "%s", "version of R of the runs")
guide_number("packages", tex_escape(sub("^deSolve", "deSolve", info[[1]]$packages[2])), "%s",
             "versions of the packages of the runs")
days <- sort(unique(vapply(info, function(i) i$day, character(1))))
guide_number("first_day", days[1], "%s", "first day of the runs listed")
guide_number("last_day", days[length(days)], "%s", "last day of the runs listed")
