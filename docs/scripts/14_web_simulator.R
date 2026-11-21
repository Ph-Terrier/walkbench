# Title: Worked example of the chapter "Web simulator" of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by a writer agent (Claude Opus 5.5) dispatched by the guide session of
#   SIM-ACI (Claude Fable 5.2), on 2026-11-21, without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: path of the package (.. in place of ../../testbed), folder web/ in the header (web/ in place of testbed/web/); folder of the usage line (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-21
# Description: reruns in walkbench 0.7.0 the R code that the web simulator exported for one
#   reference run of model M2 (ARFIMA input with d = 0, seed 1), as recorded in
#   web/validation/export_check/m2_arfima.R, and compares its outcomes with the values
#   that the page gave for the same run (page_values.json of the same folder). The printed code
#   of the rerun is the exported code, with one line broken at 80 characters; a hidden block
#   checks that it parses to the same expressions as the exported file. The hidden blocks also
#   read the constants of the page (amplitudes, presets, windows, length of a reference run,
#   limits of a file series) from web/index.html, js/wb_app.js, js/wb_i18n.js and
#   README.md, count the checks of the validation reports (results_m1.md, results_m2.md), read
#   the agreement of the four exported runs (comparison.md) and the version of walkbench that
#   it records, and register every number quoted in the chapter. The rerun uses the seed that
#   the page exported (seed 1), outside the block of seeds of the guide, so that the rerun
#   reproduces the run of the page. Since 2026-11-21 (addition by the guide session, Claude
#   Fable 5.2), a hidden block also counts the checks of the reading of a file
#   (results_files.md) and reads the test files of tests/vectors/file_vectors.json with
#   scan(file, comment.char = "#"), the call of the exported code; it writes each test file to
#   the temporary folder of the R session and removes it. The script writes no other file
#   outside generated/ and does not run the page.
# Dependencies: walkbench 0.7.0 (loaded by the driver), jsonlite, digest, utils
# Input: ../web/ (read only): index.html, README.md, js/wb_app.js, js/wb_i18n.js,
#   js/wb_input.js, js/wb_m2.js, validation/results_m1.md, validation/results_m2.md,
#   validation/results_files.md, tests/vectors/file_vectors.json,
#   validation/validation_m1.md, validation/export_check/m2_arfima.R,
#   validation/export_check/page_values.json, validation/export_check/walkbench_values.csv,
#   validation/export_check/comparison.md; ../ files named in comparison.md
# Output: generated/14_web_simulator/: snippets/*.txt, output/*.txt, numbers.tex,
#   numbers.csv, run_log.csv, session.txt
# Usage: from docs/:
#   Rscript scripts/00_run_example.R scripts/14_web_simulator.R

## ---- hidden: setup ----
library(utils)
library(jsonlite)
library(digest)
source_dir <- ".."
web_dir <- file.path(source_dir, "web")
export_dir <- file.path(web_dir, "validation", "export_check")
read_text <- function(...) {
  paste(readLines(file.path(web_dir, ...), warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}
grab <- function(pattern, text, what) {
  m <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(m) < 2L) stop("not found in the page: ", what)
  m[2]
}
num_text <- function(x) format(x, scientific = FALSE, trim = TRUE)
pow10 <- function(x) {
  # 1e-4 -> 10^{-4}; 8.4e-06 -> 8.4 \times 10^{-6} (printed in math mode)
  e <- as.integer(floor(log10(abs(x))))
  m <- signif(x / 10^e, 2)
  if (isTRUE(all.equal(m, 1))) sprintf("10^{%d}", e) else sprintf("%s \\times 10^{%d}", format(m), e)
}
html <- read_text("index.html")
app <- read_text("js", "wb_app.js")
i18n <- read_text("js", "wb_i18n.js")
readme <- read_text("README.md")

## ---- hidden: page-constants ----
# settings of each model in wb_app.js (object MODELS)
model_block <- function(id) grab(paste0(id, ": \\{([\\s\\S]*?)\\n    \\}"), app, paste("settings of", id))
setting <- function(id, key) {
  as.numeric(grab(paste0("\\b", key, ":\\s*([-0-9.e]+)"), model_block(id), paste(key, "of", id)))
}
pair <- function(id, key) {
  as.numeric(strsplit(grab(paste0("\\b", key, ":\\s*\\[([^]]*)\\]"), model_block(id),
                           paste(key, "of", id)), ",")[[1]])
}
for (id in c("M1", "M2")) {
  ref <- setting(id, "sigmaRef")
  beyond <- setting(id, "sigmaBeyond")
  slider_end <- setting(id, "sigmaMax")
  # the preset "Beyond sigma_max" takes 3 sigma_max and the slider ends at 5 sigma_max
  sigma_max <- beyond / 3
  if (abs(slider_end / 5 - sigma_max) > 1e-12) stop("slider end and preset disagree for ", id)
  low <- tolower(id)
  guide_number(paste0("sigma_ref_", low), num_text(ref), "%s", paste("sigma_ref of the page,", id))
  guide_number(paste0("sigma_max_", low), num_text(signif(sigma_max, 6)), "%s",
               paste("sigma_max of the page,", id))
  guide_number(paste0("sigma_beyond_", low), num_text(beyond), "%s",
               paste("amplitude of the preset Beyond sigma_max,", id))
  guide_number(paste0("sigma_end_", low), num_text(slider_end), "%s",
               paste("end of the slider of the amplitude,", id))
  ws <- pair(id, "s")
  wl <- pair(id, "l")
  guide_number(paste0("ws0_", low), num_text(ws[1]), "%s", paste("start of the lambda_S window,", id))
  guide_number(paste0("ws1_", low), num_text(ws[2]), "%s", paste("end of the lambda_S window,", id))
  guide_number(paste0("wl0_", low), num_text(wl[1]), "%s", paste("start of the lambda_L window,", id))
  guide_number(paste0("wl1_", low), num_text(wl[2]), "%s", paste("end of the lambda_L window,", id))
  guide_number(paste0("horizon_", low), num_text(setting(id, "horizonMin")), "%s",
               paste("smallest horizon of the reference run,", id))
}
guide_number("beyond_multiple", 3, "%d", "multiple of sigma_max of the preset Beyond sigma_max")
guide_number("slider_multiple", 5, "%d", "multiple of sigma_max at the end of the slider")
if (!grepl("uses 3 σmax", i18n, fixed = TRUE)) stop("the note of the presets names another multiple")
# hints of the page agree with the settings
if (!grepl("σref of the plan: 0.00234 rad; σmax: 0.0039 rad", i18n, fixed = TRUE) ||
    !grepl("σref of the plan: 0.00018; σmax: 0.00030 (leg lengths)", i18n, fixed = TRUE)) {
  stop("the hints of the amplitudes differ from the settings of the page")
}
# presets: memory d of each preset (index.html)
presets <- regmatches(html, gregexpr('data-p="[a-z]+,[-0-9.]+" data-i18n="preset_[a-z]+"', html))[[1]]
preset_d <- setNames(as.numeric(sub('^data-p="[a-z]+,([-0-9.]+)".*$', "\\1", presets)),
                     sub('^.*data-i18n="preset_([a-z]+)"$', "\\1", presets))
preset_level <- setNames(sub('^data-p="([a-z]+),.*$', "\\1", presets), names(preset_d))
if (!identical(names(preset_d), c("none", "white", "pers", "anti", "storm")) ||
    !identical(unname(preset_level), c("none", "ref", "ref", "ref", "beyond"))) {
  stop("the presets differ from the table of the chapter")
}
guide_number("d_pers", num_text(preset_d[["pers"]]), "%s", "memory d of the preset Persistent")
guide_number("d_anti", num_text(preset_d[["anti"]]), "%s", "memory d of the preset Anti-persistent")
if (preset_d[["none"]] != 0 || preset_d[["white"]] != 0 || preset_d[["storm"]] != 0) {
  stop("the presets No noise, White and Beyond were expected to use d = 0")
}
# slider of the memory d
d_attr <- regmatches(html, regexec('id="dmem" min="([-0-9.]+)" max="([-0-9.]+)" step="([-0-9.]+)"', html))[[1]][2:4]
guide_number("d_min", d_attr[1], "%s", "smallest memory d of the slider")
guide_number("d_max", d_attr[2], "%s", "largest memory d of the slider")
guide_number("d_step", d_attr[3], "%s", "step of the slider of the memory d")
# reference run: kept steps, redraws after a fall, upper bound of the windows
n_keep <- as.integer(grab("const N_KEEP = ([0-9]+);", app, "N_KEEP"))
n_redraws <- as.integer(grab("for \\(let a = 0; a <= ([0-9]+); a\\+\\+\\)", app, "redraw loop"))
guide_number("n_keep", n_keep, "%d", "kept steps of a reference run")
guide_number("n_redraws", n_redraws, "%d", "largest number of redraws after a fall")
guide_number("n_attempts", n_redraws + 1L, "%d", "largest number of attempts of a reference run")
if (!grepl("seedUsed = S.seed + a;", app, fixed = TRUE)) stop("the seed of an attempt was expected to be the seed plus the attempt")
if (formals(run_trial)$max_redraws != n_redraws) stop("run_trial() was expected to allow as many redraws as the page")
w_end <- as.numeric(grab("v\\[1\\] <= ([0-9.]+) &&", app, "upper bound of the windows"))
guide_number("window_end", num_text(w_end), "%s", "largest end of a fitting window")
if (!grepl("horizon: Math.max(MD().horizonMin, w.l[1])", app, fixed = TRUE)) stop("rule of the horizon changed")
# series from a file
# post-generation fix by the guide session (Claude Fable 5.2), 2026-11-21: checks of the rule
# that the page applies since 2026-11-21 (one number per line, written with a decimal point;
# the text after # is a comment; blank lines are skipped; any other line refuses the file),
# in place of the check of the former rule (last number of each line)
# post-generation fix by the guide session (Claude Fable 5.2), 2026-11-21, after the second
# change of the page of that day: the smallest number of values is read in the call of
# seriesProblem(), and the two refusals of a series (too few values, then equal values) and
# the message for equal values are checked
n_file_min <- as.integer(grab("else if \\(WB\\.input\\.seriesProblem\\(vals, ([0-9]+)\\)\\) \\{", app, "smallest file series"))
guide_number("n_file_min", n_file_min, "%d", "smallest number of values of a file series")
input_js <- read_text("js", "wb_input.js")
if (!grepl(paste0("if (values.length < nMin) return \"too_few\";\n",
                  "    if (!(WB.est.sdR(values) > 0)) return \"constant\";"), input_js, fixed = TRUE)) {
  stop("the order of the two refusals of a series differs from the text of the chapter")
}
if (!grepl(sprintf("S.fileMsg = WB.input.seriesProblem(vals, %d) === \"constant\" ? { constant: true } : { error: true };", n_file_min),
           app, fixed = TRUE) ||
    !grepl("S.fileMsg.constant ? WB.t(\"file_constant\") : S.fileMsg.error ? WB.t(\"file_error\")", app, fixed = TRUE)) {
  stop("the messages of a refused series differ from the text of the chapter")
}
msg_constant <- paste0("File refused: all values are equal, and a constant series cannot be rescaled ",
                       "to the standard deviation σ.")
msg_few <- sprintf("Fewer than %d readable values in the file.", n_file_min)
if (!grepl(paste0("file_constant: \"", msg_constant, "\""), i18n, fixed = TRUE) ||
    !grepl(paste0("file_error: \"", msg_few, "\""), i18n, fixed = TRUE)) {
  stop("the messages for equal values or for too few values differ from the table of the chapter")
}
if (!grepl("const NUMBER_RE = /^[+-]?(\\d+\\.?\\d*|\\.\\d+)([eE][+-]?\\d+)?$/;", input_js, fixed = TRUE) ||
    !grepl('const hash = lines[i].indexOf("#");', input_js, fixed = TRUE) ||
    !grepl("if (!s) continue;", input_js, fixed = TRUE) ||
    !grepl("else bad.push(", input_js, fixed = TRUE)) {
  stop("the rule of the reading of a file differs from the text of the chapter")
}
if (!grepl("if (read.bad.length) {\n        S.fileSeries = null;", app, fixed = TRUE)) {
  stop("the page was expected to refuse a file with a reported line")
}
# message of a refused file: lines with a decimal comma first, then at most this many numbers
if (!grepl("const list = comma.length ? comma : S.fileMsg.bad;", app, fixed = TRUE)) {
  stop("the choice of the lines of the message changed")
}
n_bad_lines <- as.integer(grab("list\\.slice\\(0, ([0-9]+)\\)\\.map", app, "line numbers of a message"))
guide_number("n_bad_lines", n_bad_lines, "%d", "largest number of line numbers in the message of a refused file")
msg_comma <- paste0("File refused: decimal comma in {n} line(s) (line numbers: {lines}), for example ",
                    "“{text}”. The page and the function scan() of R read numbers written with a ",
                    "decimal point (0.123).")
msg_format <- paste0("File refused: {n} line(s) do not hold a single number (line numbers: {lines}), ",
                     "for example “{text}”. Expected format: one number per line, written with a ",
                     "decimal point; the text after # is a comment.")
if (!grepl(msg_comma, i18n, fixed = TRUE) || !grepl(msg_format, i18n, fixed = TRUE)) {
  stop("the messages of a refused file differ from the table of the chapter")
}
# exported code of a series from a file: comment line on the format, then scan()
if (!grepl('L.push("# one number per line, with a decimal point; the text after # is a comment");', app, fixed = TRUE) ||
    !grepl('", comment.char = \\"#\\")");', app, fixed = TRUE)) {
  stop("the exported code of a series from a file differs from the text of the chapter")
}
# indicative values and language
n_live <- as.integer(grab("over about the last ([0-9]+) steps", i18n, "steps of the indicative values"))
guide_number("n_live", n_live, "%d", "steps of the indicative values during the animation")
if (!grepl('nl.indexOf("fr") === 0 ? "fr" : "en"', app, fixed = TRUE)) stop("rule of the first language changed")
if (!grepl('prefers-reduced-motion: reduce)").matches) setPaused(true)', app, fixed = TRUE)) stop("reduced motion rule changed")
if (!grepl("connect-src 'none'", html, fixed = TRUE)) stop("the page was expected to forbid network connections")
# integration of the page and of walkbench (README.md of web/)
sub_step <- as.numeric(grab("substeps of at most ([0-9.]+) time units", readme, "substep"))
grid_step <- as.numeric(grab("sampling grid of walkbench \\(step ([0-9.]+)\\)", readme, "grid step"))
rtol <- as.numeric(grab("relative tolerance ([0-9.e-]+)\\)", readme, "rtol"))
if (grid_step != formals(model_m1)$dt || grid_step != formals(model_m2)$dt) stop("the sampling step of the models differs from the README of the page")
guide_number("sub_step", num_text(sub_step), "%s", "longest substep of the integration of the page")
guide_number("grid_step", num_text(grid_step), "%s", "sampling step of walkbench")
guide_number("rtol", pow10(rtol), "%s", "relative tolerance of lsodar() in walkbench, after the README of the page")
deno <- grab("run under Deno ([0-9.]+) or later", readme, "version of Deno")
guide_number("deno", deno, "%s", "oldest version of Deno for the tests of the page")
# configurations of the models of the page
gamma_m1 <- formals(model_m1)$gamma
if (!grepl(sprintf("frame of the slope (γ = %s rad)", gamma_m1), i18n, fixed = TRUE)) stop("the slope of M1 differs from the page")
guide_number("gamma_m1", num_text(gamma_m1), "%s", "default slope of M1, the slope of the page")
cfg <- regmatches(app, regexec("model_m2\\(gamma = ([0-9.]+), r_f = ([0-9.]+), k = ([0-9.]+),", app))[[1]]
if (length(cfg) != 4L) stop("configuration of M2 not found in the page")
guide_number("gamma_m2", cfg[2], "%s", "slope of M2 in the page")
guide_number("rf_m2", cfg[3], "%s", "foot radius of M2 in the page")
guide_number("k_m2", cfg[4], "%s", "hip stiffness of M2 in the page")
if (!grepl(sprintf("slope γ = %s rad; feet of radius %s", cfg[2], cfg[3]), i18n, fixed = TRUE)) stop("the legend of M2 differs from its configuration")
n_cols <- length(regmatches(app, gregexpr('\\\\"(lambda_s|lambda_l|lambda_s_per_step|lambda_l_per_step|alpha_input|alpha_duration|alpha_stride_duration)\\\\"', app))[[1]])
if (n_cols != 7L) stop("the exported code was expected to select seven columns")
guide_number("n_cols", n_cols, "%d", "columns selected by the exported code")
# transient cycles of M1 and the rule of transient_cycles()
n_tr_m1 <- transient_cycles(model_m1())
guide_number("n_transient_m1", n_tr_m1, "%d", "transient cycles of M1")
guide_number("transient_tol", num_text(formals(transient_cycles)$tol), "%s", "tolerance of the rule of the transient")
guide_number("transient_min", as.integer(formals(transient_cycles)$minimum), "%d", "smallest transient")
if (!grepl("tol = tol || 0.005;", read_text("js", "wb_m2.js"), fixed = TRUE) ||
    !grepl("minimum === undefined ? 20 : minimum", read_text("js", "wb_m2.js"), fixed = TRUE)) {
  stop("the rule of the transient of the page differs from transient_cycles()")
}

## ---- hidden: timer ----
t_rerun <- Sys.time()

## ---- rerun ----
m <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
              x_init = c(0.17184140584493454, -0.27465602777778253,
                         0.3436828116898686, -0.008800993038983606))
inp <- input_arfima(634, d = 0, sd = 0.00018, seed = 1, n_transient = 34)
run <- trim_run(simulate_run(m, inp, n_cycles = 634), n_transient = 34)
s <- m$estimation_settings
s$window_s <- c(0, 0.5)
s$window_l <- c(4, 10)
s$horizon <- 10
row <- estimate_all(run, m, settings = s, input = inp)
row[, c("lambda_s", "lambda_l", "lambda_s_per_step", "lambda_l_per_step",
        "alpha_input", "alpha_duration", "alpha_stride_duration")]

## ---- hidden: checks-rerun ----
guide_number("t_rerun", as.numeric(difftime(Sys.time(), t_rerun, units = "secs")), "%.0f",
             "run time of the rerun of the exported code, in seconds")
# the printed code is the exported code: same expressions after parsing
snippet <- readLines(file.path(guide_dir(), "snippets", "rerun.txt"), warn = FALSE)
exported <- file.path(export_dir, "m2_arfima.R")
same <- identical(lapply(parse(text = snippet, keep.source = FALSE), deparse),
                  lapply(parse(exported, keep.source = FALSE), deparse))
if (!same) stop("the printed code differs from the code exported by the page")
exported_lines <- readLines(exported, warn = FALSE)
if (!any(grepl("^# Rerun in walkbench \\(version 0.7.0 or later\\)", exported_lines)) ||
    !any(grepl('^# install.packages\\("remotes"\\); remotes::install_github\\("Ph-Terrier/walkbench"\\)$', exported_lines))) {
  stop("the comment lines of the exported code differ from the text of the chapter")
}
if (!grepl('L.push("m <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,");', app, fixed = TRUE) ||
    !grepl("version 0.7.0 or later", app, fixed = TRUE)) stop("the generator of the page differs from the exported file")
x_page <- as.numeric(strsplit(grab("X_INIT_PLAN = \\[([^]]*)\\]", read_text("js", "wb_m2.js"), "X_INIT_PLAN"), ",")[[1]])
if (!identical(x_page, c(0.17184140584493454, -0.27465602777778253, 0.3436828116898686,
                         -0.008800993038983606))) stop("x_init differs from the guess of the page")
# transient of M2 in this configuration, and run without fall
n_tr_m2 <- transient_cycles(m)
if (n_tr_m2 != 34L) stop("the transient of the configuration of M2 was expected to be 34 cycles")
guide_number("n_transient_m2", n_tr_m2, "%d", "transient cycles of M2 in the configuration of the page")
if (n_tr_m1 != 20L) stop("the exported code of M1 uses 20 transient cycles")
if (isTRUE(run$fell) || row$fell) stop("the rerun was expected to complete without a fall")
if (nrow(run$events) != n_keep) stop("the rerun was expected to keep 600 steps")
# the rerun gives the values that the export check recorded with walkbench 0.7.0
recorded <- read.csv(file.path(export_dir, "walkbench_values.csv"))
rec <- recorded[recorded$case == "m2_arfima", ]
q_all <- c("lambda_s", "lambda_l", "n_pairs", "alpha_input", "alpha_duration")
if (max(abs(unlist(rec[q_all]) - unlist(row[q_all]))) > 1e-9) {
  stop("the rerun differs from walkbench_values.csv")
}
# post-generation fix by the guide session (Claude Fable 5.2), 2026-11-21, after the check of
# the reviewer agent (Claude Opus 5.5): the values of the exported run are read from the
# exported code and from the input object, in place of typed values
ex_seed <- as.integer(sub(".*seed = ([0-9]+).*", "\\1",
                          grep("input_arfima\\(", exported_lines, value = TRUE)[1]))
if (is.na(ex_seed)) stop("no seed found in the exported code")
guide_number("ex_seed", ex_seed, "%d", "seed of the exported run")
guide_number("ex_sigma", num_text(inp$params$sd), "%s", "amplitude of the exported run")
guide_number("ex_d", inp$params$d, "%d", "memory d of the exported run")
guide_number("ex_n", inp$params$n, "%d", "cycles of the exported run")

## ---- compare ----
page <- jsonlite::fromJSON(file.path(web_dir, "validation", "export_check",
                                     "page_values.json"))$cases$m2_arfima
q <- c("lambda_s", "lambda_l", "n_pairs", "alpha_input", "alpha_duration")
cmp <- data.frame(page = unlist(page[q]), walkbench = unlist(row[q]))
cmp$difference <- abs(cmp$page - cmp$walkbench)
data.frame(lapply(cmp, sprintf, fmt = "%.10g"), row.names = q)

## ---- hidden: checks-compare ----
if (page$seed != 1 || page$n_transient != 34) stop("the page values belong to another run")
if (cmp["n_pairs", "difference"] != 0) stop("the numbers of pairs were expected to be equal")
tol_lambda <- 1e-4
tol_alpha <- 1e-6
if (any(cmp[c("lambda_s", "lambda_l"), "difference"] > tol_lambda) ||
    any(cmp[c("alpha_input", "alpha_duration"), "difference"] > tol_alpha)) {
  stop("the page and walkbench differ by more than the end-to-end tolerances")
}
guide_number("ex_diff_ls", pow10(cmp["lambda_s", "difference"]), "%s", "difference of lambda_S, exported run")
guide_number("ex_diff_ll", pow10(cmp["lambda_l", "difference"]), "%s", "difference of lambda_L, exported run")
guide_number("ex_pairs", cmp["n_pairs", "walkbench"], "%d", "complete pairs of the exported run")
# tolerances of the end-to-end checks, as in the validation reports
val_m1 <- read_text("validation", "validation_m1.md")
if (!grepl("| end to end | λS and λL | walkbench | 1e-4 |", val_m1, fixed = TRUE) ||
    !grepl("| end to end | α | walkbench | 1e-6 |", val_m1, fixed = TRUE)) {
  stop("the end-to-end tolerances differ from the validation report")
}
guide_number("tol_lambda", pow10(tol_lambda), "%s", "end-to-end tolerance of lambda_S and lambda_L")
guide_number("tol_alpha", pow10(tol_alpha), "%s", "end-to-end tolerance of alpha")
if (!grepl("| dynamics | fixed point (θ, θ̇) and period | tight reference | 1e-9 |", val_m1, fixed = TRUE) ||
    !grepl("| same values as R | estimators, ARFIMA filter, fixed series, generator | walkbench and R | 1e-9 |", val_m1, fixed = TRUE) ||
    !grepl("rtol 1e-13", val_m1, fixed = TRUE)) stop("the tolerances of the validation report changed")
guide_number("tol_fixed", pow10(1e-9), "%s", "tolerance of the fixed point and of the values of R")
guide_number("tol_run", pow10(1e-7), "%s", "tolerance of the runs with a fixed input")
guide_number("rtol_tight", pow10(1e-13), "%s", "relative tolerance of the tight reference")
if (!grepl("| dynamics | run with a fixed input: step durations, their cumulative sums, sampled states | tight reference | 1e-7 |", val_m1, fixed = TRUE)) {
  stop("the tolerance of the runs changed")
}

## ---- hidden: validation-counts ----
count_checks <- function(file, prefix = "T") {
  rows <- grep(paste0("^\\| ", prefix, "[0-9]"), readLines(file.path(web_dir, "validation", file), warn = FALSE, encoding = "UTF-8"), value = TRUE)
  result <- trimws(vapply(strsplit(rows, "|", fixed = TRUE), function(x) x[length(x)], character(1)))
  tol <- trimws(vapply(strsplit(rows, "|", fixed = TRUE), function(x) x[length(x) - 1L], character(1)))
  info <- tol == "information"
  if (any(result[!info] != "pass")) stop("a check of ", file, " did not pass")
  c(total = length(rows), tolerance = sum(!info), info = sum(info))
}
counts_m1 <- count_checks("results_m1.md")
counts_m2 <- count_checks("results_m2.md")
if (!identical(unname(counts_m1), c(34L, 26L, 8L)) || !identical(unname(counts_m2), c(32L, 24L, 8L))) {
  stop("the counts of checks differ from those of the note of 2026-11-21")
}
guide_number("checks_m1", counts_m1[["total"]], "%d", "checks of M1")
guide_number("checks_tol_m1", counts_m1[["tolerance"]], "%d", "checks of M1 with a tolerance")
guide_number("checks_info_m1", counts_m1[["info"]], "%d", "checks of M1 for information")
guide_number("checks_m2", counts_m2[["total"]], "%d", "checks of M2")
guide_number("checks_tol_m2", counts_m2[["tolerance"]], "%d", "checks of M2 with a tolerance")
guide_number("checks_info_m2", counts_m2[["info"]], "%d", "checks of M2 for information")
# the reports name walkbench 0.7.0 as their reference
head_m1 <- readLines(file.path(web_dir, "validation", "results_m1.md"), n = 1L, warn = FALSE)
head_m2 <- readLines(file.path(web_dir, "validation", "results_m2.md"), n = 1L, warn = FALSE)
if (!grepl("vectors of walkbench 0.7.0", head_m1, fixed = TRUE) ||
    !grepl("vectors of walkbench 0.7.0", head_m2, fixed = TRUE)) stop("the results were expected to use the vectors of walkbench 0.7.0")

## ---- hidden: file-reading ----
# post-generation addition by the guide session (Claude Fable 5.2), 2026-11-21: checks of the
# reading of a file (results_files.md, written by tests/run_tests_files.js), and reading of the
# test files by scan() in this session, with the call of the exported code
counts_files <- count_checks("results_files.md", "F")
# 7 checks since the second change of the page of 2026-11-21 (check F7 on equal values)
if (!identical(unname(counts_files), c(7L, 6L, 1L))) {
  stop("the counts of the checks of the reading of a file differ from those of the note of 2026-11-21")
}
guide_number("checks_files", counts_files[["total"]], "%d", "checks of the reading of a file")
guide_number("checks_tol_files", counts_files[["tolerance"]], "%d", "checks of the reading of a file with a tolerance")
guide_number("checks_info_files", counts_files[["info"]], "%d", "checks of the reading of a file for information")
res_files <- readLines(file.path(web_dir, "validation", "results_files.md"), warn = FALSE, encoding = "UTF-8")
if (!grepl("vectors of walkbench 0.7.0", res_files[1], fixed = TRUE)) stop("results_files.md was expected to use the vectors of walkbench 0.7.0")
cells <- function(id) trimws(strsplit(grep(paste0("^\\| ", id, " \\|"), res_files, value = TRUE), "|", fixed = TRUE)[[1]])
f1 <- cells("F1")
f4 <- cells("F4")
f5 <- cells("F5")
f1_counts <- as.integer(regmatches(f1[3], regexec("for ([0-9]+) files \\(([0-9]+) accepted, ([0-9]+) refused\\)", f1[3]))[[1]][2:4])
f5_counts <- as.integer(regmatches(f5[3], regexec("\\(([0-9]+) of ([0-9]+);", f5[3]))[[1]][2:3])
if (anyNA(f1_counts) || anyNA(f5_counts)) stop("the counts of results_files.md could not be read")
if (f1[4] != "0") stop("the page was expected to accept and refuse the test files as its rule requires")
# the same files, read here by scan() with the call of the exported code
vec <- jsonlite::fromJSON(file.path(web_dir, "tests", "vectors", "file_vectors.json"), simplifyVector = FALSE)
if (vec$meta$scan_call != 'scan(file, comment.char = "#")') stop("the test vectors were expected to use the call of the exported code")
bits_of <- function(v) {
  vapply(v, function(x) paste(as.character(writeBin(x, raw(), endian = "big")), collapse = ""), character(1))
}
read_case <- function(case) {
  f <- tempfile(fileext = ".txt")
  on.exit(unlink(f))
  writeBin(jsonlite::base64_dec(case$bytes_base64), f)
  v <- tryCatch(scan(f, comment.char = "#", quiet = TRUE), error = function(e) NULL)
  if (is.null(v)) return(NA_integer_)
  if (length(v) && !identical(unname(bits_of(v)), unlist(case$r_bits))) {
    stop("scan() of this session reads other numbers than the test vectors: ", case$name)
  }
  length(v)
}
n_read <- vapply(vec$cases, read_case, integer(1))
page_accepts <- vapply(vec$cases, function(case) isTRUE(case$page_accepts), logical(1))
recorded_n <- vapply(vec$cases, function(case) if (isTRUE(case$r_accepts)) as.integer(case$r_n) else NA_integer_, integer(1))
if (!identical(n_read, recorded_n)) stop("scan() of this session and the test vectors disagree on a file")
if (anyNA(n_read[page_accepts])) stop("scan() was expected to read every file that the page accepts")
if (!identical(c(length(page_accepts), sum(page_accepts), sum(!page_accepts)), f1_counts)) {
  stop("the test vectors and results_files.md disagree on the numbers of files")
}
if (sum(n_read[page_accepts]) != f5_counts[2]) stop("the values of the accepted files differ from the count of check F5")
case_names <- vapply(vec$cases, function(case) case$name, character(1))
two_columns <- n_read[case_names == "two columns separated by a tab"]
if (length(two_columns) != 1L || is.na(two_columns) || two_columns != 6L) stop("scan() was expected to read the six numbers of the file with two columns")
guide_number("n_test_files", f1_counts[1], "%d", "test files of the reading of a file")
guide_number("n_files_accepted", f1_counts[2], "%d", "test files that the page accepts")
guide_number("n_files_refused", f1_counts[3], "%d", "test files that the page refuses")
guide_number("n_refused_read", sum(!is.na(n_read[!page_accepts])), "%d", "refused test files that scan() reads")
guide_number("n_values_files", format(f5_counts[2], big.mark = ",", trim = TRUE), "%s", "values of the accepted test files")
guide_number("n_values_differ", f5_counts[1], "%d", "values whose bits differ between the page and scan()")
guide_number("files_rel_diff", pow10(as.numeric(f5[4])), "%s", "largest relative difference of a value between the page and scan()")
guide_number("files_fixed_diff", pow10(as.numeric(f4[4])), "%s", "largest difference after input_fixed_series(), page and walkbench")

## ---- hidden: export-check ----
comparison <- readLines(file.path(export_dir, "comparison.md"), warn = FALSE, encoding = "UTF-8")
if (!any(grepl("walkbench 0.7.0 loaded from the source tree of the package", comparison, fixed = TRUE))) {
  stop("comparison.md was expected to record walkbench 0.7.0")
}
# SHA-256 of the files of walkbench that ran the export check, against the present source
sums <- grep("^- `[^`]+`: [0-9a-f]{64}$", comparison, value = TRUE)
files <- sub("^- `([^`]+)`: .*$", "\\1", sums)
recorded_sha <- sub("^.*: ([0-9a-f]{64})$", "\\1", sums)
# post-generation fix by the guide session (Claude Fable 5.2), 2026-11-21, on a decision of
# the author: the sums are compared without regard to the line ends. The sums of
# comparison.md are those of the files with LF line ends, and a clone of the repository may
# write the same files with CRLF line ends. Each file is read as bytes, every CR that
# precedes an LF is removed, and the SHA-256 sum of the remaining bytes is compared with the
# recorded sum. The script still stops when the content of a file differs, and its message
# names the file.
sha256_lf <- function(path) {
  bytes <- readBin(path, "raw", n = file.size(path))
  cr <- which(bytes == as.raw(0x0d))
  cr <- cr[cr < length(bytes)]
  cr <- cr[bytes[cr + 1L] == as.raw(0x0a)]
  if (length(cr)) bytes <- bytes[-cr]
  digest::digest(bytes, algo = "sha256", serialize = FALSE)
}
if (length(files) != 6L) stop("comparison.md was expected to record the sums of six files of walkbench")
present_sha <- vapply(files, function(f) sha256_lf(file.path(source_dir, f)), character(1))
changed <- files[recorded_sha != present_sha]
if (length(changed)) {
  stop("the content of ", paste(changed, collapse = ", "), " differs from the content of the ",
       "files of walkbench that ran the export check of the page (SHA-256 sums of ",
       "web/validation/export_check/comparison.md, compared without regard to the line ends)")
}
guide_number("n_sha_files", length(files), "%d", "files of walkbench whose sums comparison.md records")
rows <- grep("^\\| m[12]_(arfima|file) \\|", comparison, value = TRUE)
tab <- do.call(rbind, lapply(strsplit(rows, "|", fixed = TRUE), function(x) {
  x <- trimws(x)
  # the column "Absolute difference" was computed by check_exports.R from the full values
  data.frame(case = x[2], quantity = x[3], page = as.numeric(x[4]), walkbench = as.numeric(x[5]),
             difference = as.numeric(x[6]))
}))
if (anyNA(tab$difference)) stop("a difference of comparison.md could not be read")
if (length(unique(tab$case)) != 4L) stop("comparison.md was expected to hold four runs")
guide_number("export_runs", length(unique(tab$case)), "%d", "reference runs of the export check")
lam <- tab$quantity %in% c("lambda_s", "lambda_l")
guide_number("export_max_lambda", pow10(max(tab$difference[lam])), "%s",
             "largest difference of lambda_S or lambda_L among the four runs")
guide_number("export_max_alpha_dur", pow10(max(tab$difference[tab$quantity == "alpha_duration"])), "%s",
             "largest difference of the alpha of the step durations among the four runs")
guide_number("export_max_alpha_inp", pow10(max(tab$difference[tab$quantity == "alpha_input"])), "%s",
             "largest difference of the alpha of the input among the four runs")
if (any(tab$difference[tab$quantity == "n_pairs"] != 0)) stop("the numbers of pairs were expected to be equal in the four runs")
# the comparison file agrees with this rerun for the run of M2 with seed 1
mine <- tab[tab$case == "m2_arfima", ]
if (max(abs(mine$walkbench - unlist(row[mine$quantity])) / pmax(1, abs(mine$walkbench))) > 1e-8) {
  stop("comparison.md and the rerun differ for the run m2_arfima")
}

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
