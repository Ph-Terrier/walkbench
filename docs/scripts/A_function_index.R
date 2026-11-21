# Title: Tables of the appendix "Function index" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: path of the help pages (../man in place of ../../testbed/man); folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: writes one table per part of walkbench 0.7.0 with every exported function, the
#   title of its help page and the chapter of the guide that documents it. The functions and
#   their parts come from scripts/function_map.csv, which is checked against the namespace;
#   the titles are read from the help pages of the package (man/*.Rd), through their
#   \alias and \title entries. The script prints no code block: every block is hidden.
# Dependencies: walkbench 0.7.0 (loaded by the driver), tools, utils
# Input: scripts/function_map.csv; the help pages of the package (read only)
# Output: generated/A_function_index/: tables.tex, numbers.tex, numbers.csv, run_log.csv,
#   session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/A_function_index.R

## ---- hidden: index ----
library(tools)
library(utils)
map <- read.csv(file.path("scripts", "function_map.csv"), stringsAsFactors = FALSE)
names(map)[1] <- "fn"
exports <- sort(getNamespaceExports("walkbench"))
if (!identical(sort(map$fn), exports)) stop("scripts/function_map.csv differs from the exports")

# title of the help page that documents each function
rd_files <- list.files(file.path("..", "man"), pattern = "[.]Rd$", full.names = TRUE)
rd_text <- function(x) paste(trimws(unlist(strsplit(paste(as.character(x), collapse = ""), "\n"))),
                             collapse = " ")
titles <- list()
for (f in rd_files) {
  rd <- parse_Rd(f)
  tags <- vapply(rd, function(x) attr(x, "Rd_tag"), character(1))
  title <- trimws(gsub("\\s+", " ", rd_text(rd[[which(tags == "\\title")[1]]])))
  for (a in rd[tags == "\\alias"]) titles[[rd_text(a)]] <- title
}
missing <- setdiff(map$fn, names(titles))
if (length(missing) > 0) stop("no help page for: ", paste(missing, collapse = ", "))
map$title <- vapply(map$fn, function(f) titles[[f]], character(1))

# LaTeX text of a title: special characters escaped
tex_escape <- function(x) {
  x <- gsub("\\\\", "\\\\textbackslash{}", x)
  x <- gsub("([&%$#_{}])", "\\\\\\1", x)
  x <- gsub("~", "\\\\textasciitilde{}", x, fixed = TRUE)
  gsub("^", "\\\\textasciicircum{}", x, fixed = TRUE)
}
parts <- unique(map$part)
out <- "% written by scripts/A_function_index.R; do not edit"
for (p in parts) {
  rows <- map[map$part == p, ]
  key <- unique(rows$chapter)
  head_text <- paste0(toupper(substr(p, 1, 1)), substring(p, 2))
  where <- paste(sprintf("\\chap{%s}", key), collapse = " and ")
  out <- c(out, "",
           sprintf("\\subsection*{%s (%s)}", head_text, where),
           "\\begin{tabularx}{\\textwidth}{@{}P{5.4cm}L@{}}",
           "\\toprule", "Function & Title of the help page \\\\", "\\midrule",
           sprintf("\\fn{%s} & %s \\\\", rows$fn, tex_escape(rows$title)),
           "\\bottomrule", "\\end{tabularx}")
}
writeLines(out, file.path(guide_dir(), "tables.tex"), useBytes = TRUE)
guide_number("n_exports", nrow(map), "%d", "exported functions listed in the index")
guide_number("n_parts", length(parts), "%d", "parts of the package in the index")
guide_number("n_help_pages", length(rd_files), "%d", "help pages of the package, the page of the package included")
