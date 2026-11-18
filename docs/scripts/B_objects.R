# Title: Tables of the appendix "Objects and their fields" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-18: folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: builds one object of each kind that walkbench 0.6.2 creates (model, input, run
#   with its table of events, linearization, result of a trial, estimation settings of M1 and
#   M2, summary row) and writes, for each one, a table of its fields with the type of each
#   field as R reports it. The tables are a compact reference; the chapters describe the
#   content of the fields. The script prints no code block: every block is hidden. Seeds:
#   90,014,001 upward (cell 140 of the block of the guide).
# Dependencies: walkbench 0.6.2 (loaded by the driver), utils
# Input: none
# Output: generated/B_objects/: tables.tex, numbers.tex, numbers.csv, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/B_objects.R

## ---- hidden: objects ----
library(utils)
m1 <- model_m1()
m2 <- model_m2()
n_transient <- transient_cycles(m1)
n_keep <- 60L
n_total <- n_transient + n_keep
u <- input_iid_uniform(n_total, rho = 0.004, seed = 90014001)
run <- simulate_run(m1, u, n_cycles = n_total)
if (run$fell) stop("the run of the appendix was expected to complete")
lin <- linearize(m1)
draw <- draw_single_seed(m1, function(n, seed) input_iid_uniform(n, rho = 0.004, seed = seed),
                         n = n_total)
trial <- run_trial(m1, draw, function(attempt) seed_campaign(90000000, 140, 2, attempt),
                   n_transient = n_transient, n_keep = n_keep, cell = 140, replicate = 2,
                   keep_run = TRUE)
if (trial$fell) stop("the trial of the appendix was expected to complete")

## ---- hidden: tables ----
# type of a field, as a short text
type_of <- function(x) {
  if (is.function(x)) return("function")
  if (is.data.frame(x)) return(sprintf("data frame, %d rows and %d columns", nrow(x), ncol(x)))
  if (is.matrix(x)) return(sprintf("%s matrix, %d by %d", typeof(x), nrow(x), ncol(x)))
  if (is.array(x)) return(sprintf("%s array, %s", typeof(x), paste(dim(x), collapse = " by ")))
  if (is.null(x)) return("NULL")
  if (is.list(x)) {
    cl <- setdiff(class(x), "list")
    return(sprintf("list of %d%s", length(x), if (length(cl)) sprintf(", class %s", cl[1]) else ""))
  }
  sprintf("%s, length %d", typeof(x), length(x))
}
tex_escape <- function(x) gsub("([&%$#_{}])", "\\\\\\1", x)
field_table <- function(obj, caption, label) {
  rows <- sprintf("\\code{%s} & %s \\\\", names(obj), tex_escape(vapply(obj, type_of, character(1))))
  c("", "\\begin{table}[ht]", sprintf("\\caption{%s}", caption), sprintf("\\label{%s}", label),
    "\\small", "\\begin{tabularx}{\\textwidth}{@{}P{5.2cm}L@{}}", "\\toprule",
    "Field & Type in the example \\\\", "\\midrule", rows, "\\bottomrule", "\\end{tabularx}",
    "\\end{table}")
}
# value of a setting, as a short text
value_of <- function(x) {
  if (is.null(x)) return("NULL")
  if (is.list(x) && !is.null(names(x)) && all(vapply(x, is.atomic, logical(1)))) {
    return(paste(sprintf("%s = %s", names(x), vapply(x, function(v) paste(trimws(format(v)), collapse = ", "), "")),
                 collapse = "; "))
  }
  if (is.list(x)) return(sprintf("list of %d (%s)", length(x), paste(names(x), collapse = ", ")))
  paste(trimws(format(x)), collapse = ", ")
}
settings_table <- function() {
  s1 <- m1$estimation_settings
  s2 <- settings_m2()
  s3 <- settings_m2("continuous")
  fields <- unique(c(names(s2), names(s1), names(s3)))
  cell <- function(s, f) if (f %in% names(s)) tex_escape(value_of(s[[f]])) else "(absent)"
  rows <- sprintf("\\code{%s} & %s & %s & %s \\\\", fields,
                  vapply(fields, function(f) cell(s1, f), ""),
                  vapply(fields, function(f) cell(s2, f), ""),
                  vapply(fields, function(f) cell(s3, f), ""))
  c("", "\\begin{table}[ht]",
    "\\caption{Estimation settings of M1 (\\code{model_m1()$estimation_settings}) and of M2 (\\code{settings_m2()}, then \\code{settings_m2(\"continuous\")}); \\chap{est} describes the fields.}",
    "\\label{tab:objects-settings}", "\\footnotesize",
    "\\begin{tabularx}{\\textwidth}{@{}P{2.9cm}P{3.6cm}LP{3.6cm}@{}}", "\\toprule",
    "Field & M1 & M2 & M2, continuous \\\\", "\\midrule", rows, "\\bottomrule",
    "\\end{tabularx}", "\\end{table}")
}
out <- c(
  "% written by scripts/B_objects.R; do not edit",
  field_table(m1, "Members of a model object (class \\code{simaci_model}), here \\code{model_m1()}; \\chap{m1} describes them.", "tab:objects-model"),
  field_table(u, "Fields of an input (class \\code{simaci_input}); \\chap{port} describes them.", "tab:objects-input"),
  field_table(run, "Fields of a run (class \\code{simaci_run}) of 80 cycles of M1; \\chap{runs} describes them.", "tab:objects-run"),
  field_table(run$events, "Columns of the table \\code{events} of a run of M1; \\chap{runs} describes them.", "tab:objects-events"),
  field_table(trial, "Fields of the result of \\fn{run_trial}, called with \\code{keep_run = TRUE}; \\chap{runs} describes them.", "tab:objects-trial"),
  field_table(lin, "Fields of the linearization of M1, returned by \\fn{linearize}; \\chap{lin} describes them.", "tab:objects-lin"),
  field_table(trial$summary, "Columns of the summary row of a trial of M1; \\chap{est} describes the outcomes.", "tab:objects-row"),
  settings_table())
writeLines(out, file.path(guide_dir(), "tables.tex"), useBytes = TRUE)
if (n_total != 80L) stop("the captions state a run of 80 cycles")
guide_number("n_members", length(m1), "%d", "members of a model object")
guide_number("n_run_fields", length(run), "%d", "fields of a run")
guide_number("n_event_columns", ncol(run$events), "%d", "columns of the table of events of M1")
guide_number("n_event_columns_m2", ncol(simulate_run(m2, simaci_input(numeric(3), "zero"), n_cycles = 3)$events), "%d", "columns of the table of events of M2")
guide_number("n_row_columns", ncol(trial$summary), "%d", "columns of the summary row of a trial of M1")
