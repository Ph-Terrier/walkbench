# Title: Numbers and table of the chapter "Introduction" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: counts the exported functions of walkbench 0.6.2 and the functions of each part
#   of the package, from the namespace and from scripts/function_map.csv (function, part,
#   chapter key), and stops when the map and the namespace differ. Writes the table of the
#   parts of the package printed in the introduction. The script prints no code block: every
#   block is hidden.
# Dependencies: walkbench 0.6.2 (loaded by the driver), utils
# Input: scripts/function_map.csv
# Output: generated/01_introduction/: numbers.tex, numbers.csv, table_parts.tex, run_log.csv,
#   session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/01_introduction.R

## ---- hidden: counts ----
library(utils)
map <- read.csv(file.path("scripts", "function_map.csv"), stringsAsFactors = FALSE)
exports <- sort(getNamespaceExports("walkbench"))
if (!identical(sort(map$`function`), exports)) {
  stop("scripts/function_map.csv differs from the exports of walkbench: ",
       paste(c(setdiff(exports, map$`function`), setdiff(map$`function`, exports)),
             collapse = ", "))
}
guide_number("n_exports", length(exports), "%d", "exported functions of walkbench 0.6.2")
guide_number("n_models", length(list_models()), "%d", "models returned by list_models()")
guide_number("version", as.character(packageVersion("walkbench")), "%s", "version of the package")
guide_number("time_unit_s", sqrt(1 / 9.81), "%.2f",
             "one unit of model time, sqrt(L/g), in seconds, for a leg of 1 m and g = 9.81 m/s2")

## ---- hidden: table-parts ----
# one row per part of the package: name, content, number of exported functions, chapter
parts <- data.frame(
  part = c("models", "perturbation port", "runs and trials", "linearization",
           "closed-loop port", "estimators", "campaign rules and run log",
           "analytic predictor", "pinned code"),
  content = c(
    "the two walking models, each behind the same list of members",
    "inputs: one value per cycle, applied at a named injection point",
    "simulation of a run, removal of the transient cycles, trials with new draws after a fall, seed rule",
    "linear model of the cycle map at the fixed point, by finite differences",
    "controllers called at each contact, metronome, gain scales, stability of the linearized loop",
    "divergence exponents, Floquet multipliers, variability, detrended fluctuation analysis, summary row",
    "master seeds, cells, fall rates and inclusion of the cells, output folder, run log",
    "expected divergence levels from the linearization and the autocovariance of the input",
    "copies of code from other repositories, checked by their SHA-256 sums"),
  stringsAsFactors = FALSE)
keys <- vapply(parts$part, function(p) unique(map$chapter[map$part == p])[1], character(1))
counts <- vapply(parts$part, function(p) sum(map$part == p), integer(1))
if (sum(counts) != length(exports)) stop("the parts do not cover the exports")
# the models have two chapters: M1 (with list_models()) and M2
chap <- ifelse(parts$part == "models", "\\chap{m1}, \\chap{m2}", sprintf("\\chap{%s}", keys))
rows <- sprintf("%s%s & %s & %d & %s \\\\", toupper(substr(parts$part, 1, 1)),
                substring(parts$part, 2), parts$content, counts, chap)
writeLines(c("% written by scripts/01_introduction.R; do not edit",
             "\\begin{tabularx}{\\textwidth}{@{}P{3.4cm}LP{1.5cm}P{2.6cm}@{}}",
             "\\toprule", "Part & Content & Functions & Chapter \\\\", "\\midrule",
             rows, "\\bottomrule", "\\end{tabularx}"),
           file.path(guide_dir(), "table_parts.tex"), useBytes = TRUE)
