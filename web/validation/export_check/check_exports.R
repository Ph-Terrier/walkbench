# Title: Rerun in walkbench of the R code exported by the web simulator
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the orchestrating model of the web-simulator session of SIM-ACI
#   (Claude Opus 5.5), on 2026-11-20, without a code-generation model
# Date: 2026-11-20
# Description: writes the integer series serie_lcg.txt, sources the four R files exported by
#   the page on 2026-11-20 (m1_arfima.R, m2_arfima.R, m1_file.R, m2_file.R) with walkbench
#   loaded from the source tree of the package, and compares their outcomes with those of the
#   page (page_values.json, written by page_values.js).
#   Change of 2026-11-20 by the same model: comparison.md also records the version of walkbench
#   and the SHA-256 of the files of the package that ran.
# Dependencies: R 4.5.0 or later; pkgload, jsonlite, digest and the imports of walkbench
# Input: m1_arfima.R, m2_arfima.R, m1_file.R, m2_file.R, page_values.json (this folder)
# Output: serie_lcg.txt, walkbench_values.csv, comparison.md (this folder)
# Usage, from this folder: Rscript check_exports.R

library(pkgload)
library(jsonlite)

load_all("../../..", quiet = TRUE) # testbed/, the source tree of walkbench

# integer series: Park-Miller recurrence from 2026, then y_k = trunc(3 y_(k-1) / 5) + v_k
x <- 2026
y <- 0
vals <- numeric(600)
for (k in 1:600) {
  x <- (16807 * x) %% 2147483647
  v <- (x %% 2001) - 1000
  y <- trunc(3 * y / 5) + v
  vals[k] <- y
}
writeLines(format(vals, scientific = FALSE, trim = TRUE), "serie_lcg.txt")

cases <- c("m1_arfima", "m2_arfima", "m1_file", "m2_file")
qty <- c("lambda_s", "lambda_l", "n_pairs", "alpha_input", "alpha_duration")
wb <- list()
for (nm in cases) {
  e <- new.env()
  t0 <- Sys.time()
  sys.source(paste0(nm, ".R"), envir = e)
  r <- e$row
  wb[[nm]] <- data.frame(case = nm, r[, qty],
                         seconds = as.numeric(difftime(Sys.time(), t0, units = "secs")))
}
wb <- do.call(rbind, wb)
write.csv(wb, "walkbench_values.csv", row.names = FALSE)

page <- fromJSON("page_values.json")$cases
# record of the code of walkbench that ran (the source tree may hold uncommitted changes)
hashed <- c("DESCRIPTION", "R/estimators.R", "R/model_m1.R", "R/model_m2.R", "R/perturbation_port.R",
            "R/run_driver.R")
sha <- vapply(hashed, function(f) digest::digest(file = file.path("../../..", f), algo = "sha256"),
              character(1))
lines <- c(
  "# Export of R code: page and walkbench",
  "",
  paste0("Written by `check_exports.R` on ", format(Sys.Date()), ", ", R.version.string,
         ", walkbench ", as.character(utils::packageVersion("walkbench")),
         " loaded from the source tree of the package. Page values: `page_values.json`."),
  "",
  "SHA-256 of the files of walkbench that ran:",
  "",
  paste0("- `", hashed, "`: ", sha),
  "",
  "| Case | Quantity | Page | walkbench | Absolute difference |",
  "|---|---|---|---|---|"
)
for (nm in cases) {
  for (q in qty) {
    a <- page[[nm]][[q]]
    b <- wb[wb$case == nm, q]
    lines <- c(lines, sprintf("| %s | %s | %.10g | %.10g | %.2g |", nm, q, a, b, abs(a - b)))
  }
}
writeLines(c(lines, ""), "comparison.md")
print(wb, digits = 10)
