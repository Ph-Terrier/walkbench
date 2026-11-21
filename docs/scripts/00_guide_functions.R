# Title: Common functions of the example scripts of the walkbench user guide
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the guide session of SIM-ACI (Claude Fable 5.2), a session started by
#   the coordination session of SIM-ACI (Claude Fable 5.2), without a code-generation model
# Adapted for the public repository of walkbench by the SP1.1 session of SIM-ACI
#   (Claude Opus 5.5) on 2026-11-21: folder named in a comment (docs/ in place of
#   docs/walkbench_guide/).
# Date: 2026-11-12
# Description: functions that the example scripts of the guide call to register a number
#   quoted in the text and to draw a figure. Each number goes to numbers.tex (one LaTeX
#   definition per key, read by the macro \val of the guide) and to numbers.csv (key, printed
#   value, value with 15 significant digits, description, script). Each figure is a PDF file
#   written by an explicit graphics device, so that R leaves no Rplots.pdf. The signature of a
#   function is read from the loaded package and written to a file that the guide prints. The
#   file is sourced by 00_run_example.R, which sets the chapter and the output folder first.
# Dependencies: grDevices, graphics, utils
# Input: none
# Output: none by itself; the functions write under generated/<chapter>/

.guide <- new.env(parent = emptyenv())
.guide$numbers <- list()
.guide$chapter <- NA_character_
.guide$script <- NA_character_

# Output folder of the chapter, relative to docs/
guide_dir <- function() {
  if (is.na(.guide$chapter)) stop("the chapter is not set: run the script through 00_run_example.R")
  d <- file.path("generated", .guide$chapter)
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
  d
}

# Register a number quoted in the text of the guide.
# key: letters, digits, dots and underscores; fmt: sprintf format of the printed value
guide_number <- function(key, value, fmt, description) {
  if (!grepl("^[A-Za-z0-9._]+$", key)) stop("invalid key: ", key)
  if (length(value) != 1L || is.na(value)) stop("the number ", key, " must be one value, not missing")
  if (!is.null(.guide$numbers[[key]])) stop("the number ", key, " is registered twice")
  printed <- sprintf(fmt, value)
  full <- if (is.numeric(value)) format(value, digits = 15) else as.character(value)
  .guide$numbers[[key]] <- data.frame(key = key, printed = printed, value = full,
                                      description = description, script = .guide$script,
                                      stringsAsFactors = FALSE)
  invisible(printed)
}

# Write numbers.tex and numbers.csv of the chapter
guide_write_numbers <- function() {
  tab <- do.call(rbind, .guide$numbers)
  if (is.null(tab)) tab <- data.frame(key = character(0), printed = character(0),
                                      value = character(0), description = character(0),
                                      script = character(0))
  utils::write.csv(tab, file.path(guide_dir(), "numbers.csv"), row.names = FALSE)
  esc <- function(x) gsub("%", "\\\\%", x)
  lines <- c(sprintf("%% Numbers of chapter %s, written by scripts/%s; do not edit",
                     .guide$chapter, .guide$script),
             sprintf("\\wbnumdef{%s.%s}{%s}", sub("_.*$", "", .guide$chapter), tab$key,
                     esc(tab$printed)))
  writeLines(lines, file.path(guide_dir(), "numbers.tex"), useBytes = TRUE)
  invisible(nrow(tab))
}

# Draw one figure of the guide as a PDF file with embedded fonts.
# draw: function without argument that draws on the open device
guide_figure <- function(name, width_cm, height_cm, draw) {
  path <- file.path(guide_dir(), paste0("fig_", name, ".pdf"))
  if (isTRUE(capabilities("cairo"))) {
    grDevices::cairo_pdf(path, width = width_cm / 2.54, height = height_cm / 2.54,
                         pointsize = 9)
  } else {
    grDevices::pdf(path, width = width_cm / 2.54, height = height_cm / 2.54, pointsize = 9)
  }
  ok <- FALSE
  on.exit({
    grDevices::dev.off()
    if (!ok) unlink(path)
  })
  draw()
  ok <- TRUE
  invisible(path)
}

# Write the signature of an exported function, as the package defines it, to
# signatures/<name>.txt; the guide prints the file at the head of the reference entry
guide_signature <- function(name) {
  a <- formals(get(name, mode = "function"))
  parts <- vapply(names(a), function(n) {
    # an argument without default holds the empty symbol, which cannot be assigned
    if (is.symbol(a[[n]]) && !nzchar(as.character(a[[n]]))) {
      n
    } else {
      paste(n, "=", paste(deparse(a[[n]]), collapse = " "))
    }
  }, character(1))
  # lines of at most 78 characters, continuation lines indented by 4 spaces
  # a function without argument has the signature "name()"
  sig <- paste0(name, if (length(parts) == 0L) "()" else "(")
  for (i in seq_along(parts)) {
    piece <- paste0(parts[i], if (i < length(parts)) "," else ")")
    last <- sig[length(sig)]
    joined <- if (grepl("[(]$", last)) paste0(last, piece) else paste(last, piece)
    if (nchar(joined) <= 78L) sig[length(sig)] <- joined else sig <- c(sig, paste0("    ", piece))
  }
  d <- file.path(guide_dir(), "signatures")
  dir.create(d, showWarnings = FALSE)
  writeLines(sig, file.path(d, paste0(name, ".txt")), useBytes = TRUE)
  invisible(sig)
}

# Colors of the figures: three hues that stay distinct in grayscale and for the usual forms
# of color-vision deficiency (Okabe and Ito palette), and two grays for annotations
guide_colors <- function() {
  c(blue = "#0072B2", vermilion = "#D55E00", green = "#009E73", gray = "#6B6B6B",
    light = "#E6E6E6")
}
