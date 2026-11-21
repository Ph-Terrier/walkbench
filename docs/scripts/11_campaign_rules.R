# Title: Worked example of the chapter "Campaign rules and run log" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent of the guide session of SIM-ACI (Claude Fable 5.2),
#   dispatched by the guide session of SIM-ACI (Claude Fable 5.2), without a code-generation
#   model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: path of the package in a check of write_campaign_log() (.. in place of ../../testbed); folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-13
# Description: applies the campaign rules of walkbench 0.7.0 to a demonstration computed for
#   the guide: the table of the seven campaigns, the numbering of the cells of a design, the
#   check of the seed blocks, six trials of model M1 in two cells that differ by the amplitude
#   rho of a uniform uncorrelated input (cells 111 and 112, three replicates each, 20 transient
#   and 100 kept cycles), the table of the trials, the summary of the cells, the check of the
#   output folder and the run log. The seeds follow the seed rule with the master seed
#   90,000,000, in the block 90,000,000 to 92,099,999 of the guide, which check_seed_plan()
#   finds apart from the seed blocks of the SIM-ACI campaigns. The example prints the
#   bookkeeping columns of the trials only: no divergence exponent, no Floquet multiplier and
#   no scaling exponent. The run log and the table of the trials are written into a temporary
#   folder, which the script removes: the log records the host name and absolute paths. The
#   blocks whose label starts with "hidden:" write the signatures of the functions, register
#   the numbers quoted in the chapter, draw its figure and run the checks on which the text
#   relies; the other blocks are printed in the chapter with their output.
# Dependencies: walkbench 0.7.0 (loaded by the driver), jsonlite, stats, graphics, grDevices,
#   utils
# Input: none
# Output: generated/11_campaign_rules/: snippets/*.txt, output/*.txt, signatures/*.txt,
#   numbers.tex, numbers.csv, fig_seed_blocks.pdf, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/11_campaign_rules.R

## ---- hidden: setup ----
library(stats)
library(graphics)
library(grDevices)
library(utils)
if (!isTRUE(check_seed_plan(90000000))) stop("the seed block of the guide overlaps another block")
for (f in c("campaign_table", "campaign_master", "bootstrap_seed", "number_cells",
            "check_seed_plan", "bind_trials", "summarize_cells", "check_output_folder",
            "write_campaign_log")) guide_signature(f)
# integer with a comma between the thousands, for the numbers of the text
with_commas <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)

## ---- campaigns ----
campaign_table()
campaign_master(3)
bootstrap_seed()
number_cells(d = c(0, 0.2, 0.4), sd = c(0.001, 0.002))

## ---- seed-plan ----
check_seed_plan(90000000)
bad <- check_seed_plan(c(69000000L, 109000000L))
bad

## ---- hidden: checks-seed-plan ----
# the blocks that check_seed_plan() protects, typed from its code; each bound is then checked
# by calls of the function at the bound and next to it
span <- 2099999
protected <- rbind(
  data.frame(name = "feasibility study", lo = -100000, hi = span),
  data.frame(name = paste("replication runs, master", c(10000000, 12200000, 30000000)),
             lo = c(10000000, 12200000, 30000000),
             hi = c(10000000, 12200000, 30000000) + span),
  data.frame(name = paste("campaign", campaign_table()$j), lo = campaign_table()$master,
             hi = campaign_table()$master + span),
  data.frame(name = "surrogates", lo = 70001001, hi = 70060999))
meets <- function(master, name) {
  res <- check_seed_plan(master)
  !isTRUE(res) && name %in% attr(res, "conflicts")$b
}
for (i in seq_len(nrow(protected))) {
  ok <- meets(protected$hi[i], protected$name[i]) &&
    !meets(protected$hi[i] + 1, protected$name[i]) &&
    meets(protected$lo[i] - span, protected$name[i]) &&
    !meets(protected$lo[i] - span - 1, protected$name[i])
  if (!ok) stop("bounds of the block ", protected$name[i], " differ from those of the text")
}
if (!identical(dim(attr(bad, "conflicts")), c(3L, 2L))) stop("three conflicts were expected")
# two masters closer than 2,100,000 meet each other; two equal masters are not reported
near <- check_seed_plan(c(50000000L, 52099999L))
if (isTRUE(near) || nrow(attr(near, "conflicts")) != 2L) stop("two conflicts were expected")
if (!isTRUE(check_seed_plan(c(50000000L, 52100000L)))) stop("blocks 2,100,000 apart were expected to pass")
if (!isTRUE(check_seed_plan(c(50000000L, 50000000L)))) stop("two equal masters were expected to pass")
# a master given as a double prints in the format of paste()
if (!identical(attr(check_seed_plan(69000000), "conflicts")$a, "calibration, master 6.9e+07")) {
  stop("the name of a master given as a double was expected in scientific notation")
}
guide_number("seed_span", with_commas(span + 1), "%s", "number of seeds that one master seed covers")
guide_number("seed_span_last", with_commas(span), "%s", "offset of the last seed of the block of a master seed")
guide_number("n_conflicts", nrow(attr(bad, "conflicts")), "%d", "conflicts of the two masters of the example")

## ---- trials ----
m1 <- model_m1()
n_transient <- transient_cycles(m1)
n_keep <- 100L
n_total <- n_transient + n_keep
cells <- number_cells(rho = c(0.004, 0.028))
cells$cell <- cells$cell + 110L   # cells 111 and 112 of the block of the guide
cells$planned <- 3L
cells
one_trial <- function(cell, rho, replicate) {
  draw <- draw_single_seed(m1, function(n, seed) {
    input_iid_uniform(n, rho = rho, seed = seed)
  }, n = n_total)
  seed_of <- function(attempt) {
    seed_campaign(90000000, cell, replicate, attempt)
  }
  run_trial(m1, draw, seed_of, n_transient = n_transient, n_keep = n_keep,
            cell = cell, replicate = replicate)
}
start <- Sys.time()
trials <- list()
for (i in seq_len(nrow(cells))) {
  for (r in seq_len(cells$planned[i])) {
    trials[[length(trials) + 1L]] <- one_trial(cells$cell[i], cells$rho[i], r)
  }
}
end <- Sys.time()
trials[[4]]$seeds

## ---- bind ----
rows <- bind_trials(trials)
rows[, c("cell", "replicate", "attempts", "n_redraws", "fell", "n_falls",
         "n_numerical_failures")]

## ---- summarize ----
by_cell <- summarize_cells(rows, cells[, c("cell", "planned")])
print(by_cell, digits = 3)

## ---- hidden: numbers-trials ----
guide_number("n_transient", n_transient, "%d", "transient cycles of M1, transient_cycles(model_m1())")
guide_number("n_keep", n_keep, "%d", "kept cycles of the example")
guide_number("rho_low", 1000 * cells$rho[1], "%.0f", "rho of cell 111, in 1e-3 rad")
guide_number("rho_high", 1000 * cells$rho[2], "%.0f", "rho of cell 112, in 1e-3 rad")
guide_number("n_trials", length(trials), "%d", "trials of the example")
guide_number("n_planned", cells$planned[1], "%d", "planned replicates of each cell")
guide_number("n_columns_rows", ncol(rows), "%d", "columns of the table of the trials")
guide_number("trial4_attempts", length(trials[[4]]$seeds), "%d", "attempts of replicate 1 of cell 112")
guide_number("attempts_high", by_cell$attempts[2], "%.0f", "attempts of cell 112")
guide_number("falls_high", by_cell$falls[2], "%.0f", "falls of cell 112")
guide_number("fall_rate_high_pct", 100 * by_cell$fall_rate[2], "%.0f", "fall rate of cell 112, in percent")
guide_number("with_outcomes_high", by_cell$with_outcomes[2], "%d", "replicates with outcomes of cell 112")
guide_number("n_summary_columns", ncol(by_cell), "%d", "columns of the cell summary")
# facts on which the text relies
if (by_cell$falls[1] != 0 || by_cell$flag[1]) stop("cell 111 was expected without a fall and without a flag")
if (!all(rows$attempts[rows$cell == 111] == 1L)) stop("every trial of cell 111 was expected to complete at its first attempt")
if (any(diff(trials[[4]]$seeds) != 100000)) stop("the seeds of a trial were expected to increase by 100,000 at each attempt")
if (by_cell$falls[2] < 1 || !by_cell$flag[2] || !by_cell$flag_fall_rate[2]) stop("cell 112 was expected with falls and with both flags")
if (!all(by_cell$included) || any(rows$fell)) stop("every trial was expected to have outcomes")
if (any(by_cell$numerical_failures != 0)) stop("no numerical failure was expected")
if (!identical(rows$attempts, rows$n_redraws + 1L)) stop("attempts was expected to equal n_redraws + 1")
if (!identical(trials[[4]]$seeds, as.numeric(seed_campaign(90000000, 112, 1, 0:3)))) stop("the seeds of trial 4 differ from the seed rule")
if (length(trials[[4]]$seeds) < 2L) stop("replicate 1 of cell 112 was expected to need a redraw")
# a trial depends on its seeds only: the same trial, run again after the others, gives the
# same summary row
again <- one_trial(cells$cell[2], cells$rho[2], 1L)
if (!identical(again$summary, trials[[4]]$summary) || !identical(again$seeds, trials[[4]]$seeds)) {
  stop("a trial run again was expected to give the same seeds and the same summary row")
}
# a planned cell without a row, and rows without the columns of the falls and failures
extra_cell <- summarize_cells(rows, data.frame(cell = c(111, 112, 113), planned = 3))
if (extra_cell$attempts[3] != 0 || !is.na(extra_cell$fall_rate[3]) || extra_cell$included[3] ||
    extra_cell$flag[3]) stop("a planned cell without a row was expected with 0 attempts, no rate and no inclusion")
old_rows <- summarize_cells(rows[, c("cell", "replicate", "fell", "n_redraws")], 3)
if (any(old_rows$falls != tapply(rows$n_redraws + rows$fell, rows$cell, sum)) ||
    any(old_rows$numerical_failures != 0)) {
  stop("rows without n_falls were expected to count every attempt without outcomes as a fall")
}
if (!is.null(bind_trials(list()))) stop("bind_trials() of an empty list was expected to return NULL")

## ---- log ----
root <- tempfile("walkbench_guide_")
root_abs <- check_output_folder(root)   # absolute path; no folder is created
dir.exists(root)
out_dir <- file.path(root, "campaign")
dir.create(out_dir, recursive = TRUE)
check_output_folder(out_dir)            # an empty folder passes
log_path <- write_campaign_log(
  out_dir, campaign = "example of chapter 11 of the user guide",
  purpose = "bookkeeping of two cells of M1 with a uniform input",
  script = "11_campaign_rules.R",
  producer = "writer agent of the guide session of SIM-ACI (Claude Fable 5.2)",
  master_seed = 90000000,
  design = list(cells = cells, n_transient = n_transient, n_keep = n_keep),
  model = m1, injection = m1$default_injection,
  settings = m1$estimation_settings, start = start, end = end, runs = rows)
list.files(out_dir)
log <- jsonlite::fromJSON(log_path)
length(log)
str(log[c("campaign", "master_seed", "pinned_files_identical", "rng_kind",
          "r_version")])
unlist(log$package[c("name", "version")])

## ---- hidden: checks-log ----
guide_number("n_log_fields", length(log), "%d", "fields of campaign_log.json without the argument extra")
# the fields of the log, in the order of the table of the chapter
fields_of_table <- c("campaign", "purpose", "script", "producer", "master_seed", "design", "model",
                     "injection", "estimation_settings", "package", "campaign_repository",
                     "whole_repository", "pinned_files_identical", "r_version", "platform", "rng_kind",
                     "package_versions", "host", "start", "end", "wall_time_s")
if (!identical(names(log), fields_of_table)) stop("the fields of the log differ from the table of the chapter")
if (!identical(names(log$model), c("id", "label", "adapter_version", "params")) ||
    !identical(names(log$package), c("name", "version", "commit", "modified_files", "loaded_from")) ||
    !identical(names(log$campaign_repository), c("commit", "modified_files")) ||
    !identical(names(log$whole_repository), c("commit", "modified_files")) ||
    !identical(names(log$package_versions), c("deSolve", "RANN", "MASS", "digest", "jsonlite", "pkgload", "parallel"))) {
  stop("the elements of the fields model, package or package_versions differ from the table of the chapter")
}
if (!identical(basename(log_path), "campaign_log.json")) stop("the log was expected in campaign_log.json")
if (!identical(log$package$loaded_from, "installed")) stop("loaded_from was expected to be \"installed\" without package_path")
if (nrow(utils::read.csv(file.path(out_dir, "runs.csv"))) != nrow(rows)) stop("runs.csv was expected to hold one row per trial")
# without git: the option names a file that does not exist, so no commit is found
old_option <- options(walkbench.git = file.path(root, "no_git.exe"))
log_no_git <- write_campaign_log(
  file.path(root, "no_git"), campaign = "check", purpose = "check", script = "none",
  producer = "check", master_seed = 90000000, design = list(), model = m1,
  injection = m1$default_injection, settings = m1$estimation_settings, start = start,
  end = end, package_path = "..", extra = list(note = "extra"))
options(old_option)
json_no_git <- readLines(log_no_git, warn = FALSE)
if (sum(grepl("\"commit\": null", json_no_git, fixed = TRUE)) != 3L ||
    sum(grepl("\"modified_files\": null", json_no_git, fixed = TRUE)) != 3L) {
  stop("without git, the three commits and the three lists of modified files were expected to be null")
}
parsed_no_git <- jsonlite::fromJSON(log_no_git)
if (identical(parsed_no_git$package$loaded_from, "installed")) stop("with package_path, loaded_from was expected to be a path")
if (!identical(tail(names(parsed_no_git), 1), "note")) stop("the fields of extra were expected at the end of the log")
if (file.exists(file.path(root, "no_git", "runs.csv"))) stop("no runs.csv was expected without the argument runs")
unlink(file.path(root, "no_git"), recursive = TRUE)
# with git, a folder outside any repository: no commit, and an empty list of modified files
# (checked only when git is found on this computer)
if (!is.na(walkbench:::find_git())) {
  outside <- file.path(root, "outside")
  log_outside <- write_campaign_log(
    outside, campaign = "check", purpose = "check", script = "none", producer = "check",
    master_seed = 90000000, design = list(), model = m1, injection = m1$default_injection,
    settings = m1$estimation_settings, start = start, end = end, campaign_path = outside)
  json_outside <- paste(readLines(log_outside, warn = FALSE), collapse = " ")
  if (!grepl("\"campaign_repository\": \\{\\s*\"commit\": null,\\s*\"modified_files\": \\[\\]", json_outside)) {
    stop("outside a repository, a null commit and an empty list of modified files were expected")
  }
  unlink(outside, recursive = TRUE)
  commit <- log$campaign_repository$commit
  if (!is.null(commit) && !grepl("^[0-9a-f]{40}$", commit)) stop("a commit was expected as 40 hexadecimal characters")
}

## ---- folder-checks ----
message_of <- function(expr) {
  msg <- tryCatch({ expr; "no error" }, error = conditionMessage)
  writeLines(strwrap(gsub(root_abs, "<root>", msg, fixed = TRUE), width = 76))
}
message_of(check_output_folder(out_dir))
message_of(check_output_folder(out_dir, overwrite = TRUE))
message_of(check_output_folder(file.path(out_dir, "runs.csv")))
message_of(check_output_folder(file.path(out_dir, "smoke"),
                               forbidden_root = root))
unlink(root, recursive = TRUE)
file.exists(root)

## ---- hidden: checks-folder ----
if (file.exists(root)) stop("the temporary folder was expected to be removed")
# post-generation fix by Claude Fable 5.2, 2026-11-14: check of a statement added to the chapter
# (fix 11-1 of review/pass2_fixes.json). check_pinned_files() reads each pinned file with the
# internal function sha256_lf(); a missing file stops that reading
missing_pinned <- suppressWarnings(tryCatch({
  walkbench:::sha256_lf(file.path(tempdir(), "absent_pinned_file.R"))
  "no error"
}, error = conditionMessage))
if (!identical(missing_pinned, "cannot open the connection")) {
  stop("a missing pinned file was expected to stop with: cannot open the connection")
}
# a folder whose name starts with the name of the forbidden folder lies outside it; the
# comparison ignores the case on Windows; neither folder is created
if (!identical(tryCatch({ check_output_folder(paste0(root, "_b"), forbidden_root = root); "ok" },
                        error = conditionMessage), "ok")) stop("a sibling folder was expected to pass")
if (.Platform$OS.type == "windows") {
  msg <- tryCatch({ check_output_folder(toupper(file.path(root, "x")), forbidden_root = root); "ok" },
                  error = conditionMessage)
  if (!grepl("lies inside", msg, fixed = TRUE)) stop("the comparison was expected to ignore the case")
}
if (dir.exists(paste0(root, "_b")) || dir.exists(root)) stop("check_output_folder() was expected to create no folder")

## ---- hidden: figure-seed-blocks ----
col <- guide_colors()
guide_figure("seed_blocks", width_cm = 15.5, height_cm = 4.5, draw = function() {
  par(mar = c(3.3, 0.4, 0.3, 0.4), mgp = c(2.1, 0.6, 0), tcl = -0.3, las = 1)
  plot(NA, xlim = c(-5, 177), ylim = c(0, 3.6), axes = FALSE, xlab = "Seed (millions)",
       ylab = "", xaxs = "i", yaxs = "i")
  axis(1, at = seq(0, 170, by = 10), col = col[["gray"]], col.axis = "black")
  segments(100, 0, 100, 3.25, lty = 2, col = col[["gray"]])
  text(100, 3.42, "limit of 100,000,000", cex = 0.9)
  wide <- protected$name != "surrogates"
  rect(protected$lo[wide] / 1e6, 0, (protected$hi[wide] + 1) / 1e6, 0.9, col = col[["blue"]],
       border = "white", lwd = 0.5)
  # the block of the surrogates is 59,999 seeds wide: a line marks it
  segments(70.03, 0, 70.03, 0.9, col = col[["blue"]], lwd = 1)
  rect(90, 0, 92.1, 0.9, col = col[["vermilion"]], border = "white", lwd = 0.5)
  # bracket over a group of blocks, leader and label; drop: length of the gap under the label
  label <- function(x0, x1, y, txt, at = (x0 + x1) / 2, adj = 0.5, drop = 0.24) {
    segments(x0, 1.12, x1, 1.12, col = col[["gray"]], lwd = 0.8)
    segments((x0 + x1) / 2, 1.12, (x0 + x1) / 2, y - drop, col = col[["gray"]], lwd = 0.8)
    text(at, y, txt, cex = 0.9, adj = c(adj, 0.5))
  }
  label(-0.1, 2.1, 2.65, "feasibility study", at = -4.5, adj = 0)
  label(10, 32.1, 1.8, "replication runs")
  label(69.6, 70.5, 1.8, "surrogates")
  label(90, 92.1, 2.45, "examples of\nthis guide", at = 97.5, adj = 1, drop = 0.42)
  label(110, 172.1, 1.8, "campaigns 1 to 7")
})

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
