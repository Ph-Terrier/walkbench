# Title: Campaign rules of the frozen analysis plan: master seeds, cells, seed blocks, cell summaries
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model, on 2026-11-06, for walkbench 0.4.0 (work order of the coordination
#   session after the decision of the author of 2026-11-06 at 20:23)
# Date: 2026-11-06
# Description: rules of the SIM-ACI analysis plan frozen at commit ba72afe and registered on
#   OSF (osf.io/m74tk): master seed of campaign j = 100,000,000 + 10,000,000 j, j = 1 to 7
#   (section 4.3); cells numbered from 1 in the order of the factor levels, with the first
#   factor (d) varying fastest (section 4.3); seed of the bootstrap 20261106 (sections 4.3 and
#   6.2); seed blocks of the calibration runs kept apart from those of the campaigns, of the
#   surrogates and of the earlier runs (sections 4.3 and 4.5); fall rate = falls / attempts,
#   flag above 20%, cell excluded below 80% of planned replicates with outcomes (section 4.4).
#   walkbench 0.6.0 (2026-11-06): numerical failures counted apart, combined rate and flag of
#   section 4 of update 1 of the registration osf.io/m74tk (summarize_cells()).
# Dependencies: none
# Input: none
# Output: none

#' Campaigns of the analysis plan and their master seeds
#'
#' The campaigns are numbered as in section 4.3 of the analysis plan frozen at commit ba72afe:
#' 1 replication runs, 2 real stride series, 3 crossed design, 4 AR family, 5 variant H3a,
#' 6 variant H3b, 7 series-length runs. The master seed of campaign j of a model is
#' 100,000,000 + 10,000,000 j. The replication runs of M1 and M2 kept the seeds of their
#' scripts; the rule applies to the campaigns that follow, including the replication runs of
#' M3 and M4.
#'
#' @return A data frame with the columns `j`, `campaign` and `master`.
#' @export
campaign_table <- function() {
  j <- 1:7
  data.frame(j = j,
             campaign = c("replication runs", "real stride series", "crossed design",
                          "AR family", "variant H3a", "variant H3b", "series-length runs"),
             master = as.integer(1e8 + 1e7 * j))
}

#' Master seed of a campaign
#'
#' @param j number of the campaign, from 1 to 7 ([campaign_table()]).
#' @return The master seed, an integer.
#' @export
campaign_master <- function(j) {
  if (length(j) != 1L || !(j %in% 1:7)) stop("j must be one of 1 to 7")
  campaign_table()$master[j]
}

#' Seed of the bootstrap of section 6.2
#'
#' @return 20261106, the seed of the bootstrap of the analysis plan (sections 4.3 and 6.2).
#' @export
bootstrap_seed <- function() 20261106L

#' Cells of a campaign, numbered from 1
#'
#' The cells are numbered in the order of the factor levels, with the first factor varying
#' fastest (analysis plan, section 4.3: d varies fastest). The factors are given in that order.
#'
#' @param ... factors as named vectors of levels, the fastest first, for example
#'   `number_cells(d = d_levels, sigma = sigma_levels)`.
#' @return A data frame with the column `cell` (from 1) and one column per factor.
#' @export
number_cells <- function(...) {
  f <- list(...)
  if (length(f) == 0L || is.null(names(f)) || any(!nzchar(names(f)))) {
    stop("give the factors as named vectors of levels")
  }
  g <- expand.grid(f, KEEP.OUT.ATTRS = FALSE, stringsAsFactors = FALSE)
  cbind(cell = seq_len(nrow(g)), g)
}

#' Check the seed blocks of the calibration runs
#'
#' A master seed m covers the seeds m to m + 2,099,999 under the rule of [seed_campaign()]
#' (attempt up to 20, cell up to 999, replicate up to 99). The function checks that the
#' blocks of the calibration masters lie below 100,000,000 and do not overlap each other, the
#' blocks of the seven campaigns, the seeds of the surrogates (70,000,000 + 1,000 s + i) or
#' the seeds of the earlier runs: the feasibility study (rule of its scripts 02 and 04, seeds
#' within -100,000 to 2,099,999) and the replication runs of M1 and M2 (masters 10,000,000,
#' 12,200,000 and 30,000,000).
#'
#' @param calibration_masters master seeds of the calibration runs, for example
#'   `c(50000000, 53000000)` for M2.
#' @param n_series number of series with surrogates (60 Hausdorff series).
#' @return `TRUE` when no block overlaps; otherwise `FALSE`, with the conflicts in the
#'   attribute `"conflicts"`.
#' @export
check_seed_plan <- function(calibration_masters, n_series = 60L) {
  span <- 2099999
  blocks <- rbind(
    data.frame(name = "feasibility study", lo = -100000, hi = span),
    data.frame(name = paste("replication runs, master", c(10000000, 12200000, 30000000)),
               lo = c(10000000, 12200000, 30000000), hi = c(10000000, 12200000, 30000000) + span),
    data.frame(name = paste("campaign", campaign_table()$j), lo = campaign_table()$master,
               hi = campaign_table()$master + span),
    data.frame(name = "surrogates", lo = 70000000 + 1000 + 1,
               hi = 70000000 + 1000 * n_series + 999))
  cal <- data.frame(name = paste("calibration, master", calibration_masters),
                    lo = calibration_masters, hi = calibration_masters + span)
  out <- list()
  if (any(calibration_masters >= 1e8)) {
    out[[length(out) + 1L]] <- data.frame(a = cal$name[calibration_masters >= 1e8],
                                          b = "limit of 100,000,000")
  }
  all_b <- rbind(blocks, cal)
  for (i in seq_len(nrow(cal))) {
    for (k in seq_len(nrow(all_b))) {
      if (all_b$name[k] == cal$name[i]) next
      if (cal$lo[i] <= all_b$hi[k] && all_b$lo[k] <= cal$hi[i]) {
        out[[length(out) + 1L]] <- data.frame(a = cal$name[i], b = all_b$name[k])
      }
    }
  }
  if (length(out) == 0L) return(TRUE)
  structure(FALSE, conflicts = unique(do.call(rbind, out)))
}

#' Draw function for a fixed input
#'
#' For an input that is not redrawn (a real stride series or a surrogate): after
#' `set.seed(seed)`, only the initial jitter of the post-contact state is drawn
#' (specification, section 10.2).
#'
#' @param model a `simaci_model`.
#' @param input a `simaci_input`.
#' @param jitter_sd standard deviation of the jitter added to each state variable.
#' @return A function `function(seed)` that returns `list(input, y0)`.
#' @export
draw_fixed <- function(model, input, jitter_sd = 1e-9) {
  function(seed) {
    set.seed(seed)
    y0 <- model$fixed_point()$y0
    list(input = input, y0 = y0 + stats::rnorm(length(y0)) * jitter_sd)
  }
}

#' Summary rows of several trials in one data frame
#'
#' @param trials list of results of [run_trial()].
#' @return A data frame with one row per trial; the rows of trials that fell have missing
#'   outcomes. The column `attempts` gives the number of attempts of each trial.
#' @export
bind_trials <- function(trials) {
  rows <- lapply(trials, function(tr) {
    r <- tr$summary
    r$attempts <- length(tr$seeds)
    r
  })
  cols <- unique(unlist(lapply(rows, names)))
  rows <- lapply(rows, function(r) {
    miss <- setdiff(cols, names(r))
    for (m in miss) r[[m]] <- NA
    r[, cols]
  })
  do.call(rbind, rows)
}

#' Falls and inclusion of the cells of a campaign
#'
#' Rules of section 4.4 of the analysis plan frozen at commit ba72afe and of section 4 of its
#' update 1 (osf.io/m74tk). The fall rate of a cell is the number of falls divided by the
#' number of attempts. Numerical failures of the integrator are counted apart, and the rate of
#' falls and numerical failures combined is reported. A cell is flagged (`flag`) when the fall
#' rate or the combined rate exceeds 20%; `flag_fall_rate` keeps the flag on falls alone. A
#' cell enters the analyses of section 6 when at least 80% of its planned replicates have
#' outcomes.
#'
#' @param rows data frame of the trials, from [bind_trials()], with the columns `cell`,
#'   `replicate`, `fell` and `n_redraws`, and the columns `n_falls` and
#'   `n_numerical_failures` of walkbench 0.6.0; without them, the attempts that did not end
#'   with outcomes count as falls.
#' @param planned planned number of replicates: one number for every cell, or a data frame
#'   with the columns `cell` and `planned`.
#' @return A data frame with one row per cell: `planned`, `with_outcomes`, `attempts`, `falls`,
#'   `numerical_failures`, `fall_rate`, `numerical_failure_rate`, `combined_rate`,
#'   `flag_fall_rate`, `flag`, `share_with_outcomes` and `included`.
#' @export
summarize_cells <- function(rows, planned) {
  need <- c("cell", "replicate", "fell", "n_redraws")
  if (!all(need %in% names(rows))) stop("rows needs the columns ", paste(need, collapse = ", "))
  if (is.data.frame(planned)) {
    pl <- planned
  } else {
    if (length(planned) != 1L) stop("planned must be one number or a data frame")
    pl <- data.frame(cell = sort(unique(rows$cell)), planned = planned)
  }
  counted <- all(c("n_falls", "n_numerical_failures") %in% names(rows))
  out <- do.call(rbind, lapply(seq_len(nrow(pl)), function(i) {
    r <- rows[rows$cell == pl$cell[i], , drop = FALSE]
    ok <- sum(!r$fell)
    att <- sum(r$n_redraws + 1)
    # rows of walkbench 0.6.0 count falls and numerical failures; older rows count falls only
    falls <- if (counted) sum(r$n_falls) else sum(r$n_redraws) + sum(r$fell)
    fails <- if (counted) sum(r$n_numerical_failures) else 0
    data.frame(cell = pl$cell[i], planned = pl$planned[i], with_outcomes = ok,
               attempts = att, falls = falls, numerical_failures = fails,
               fall_rate = if (att > 0) falls / att else NA_real_,
               numerical_failure_rate = if (att > 0) fails / att else NA_real_,
               combined_rate = if (att > 0) (falls + fails) / att else NA_real_)
  }))
  out$flag_fall_rate <- !is.na(out$fall_rate) & out$fall_rate > 0.20
  # update 1 of the registration, section 4: flagged when the fall rate or the combined rate
  # of falls and numerical failures exceeds 20%
  out$flag <- out$flag_fall_rate | (!is.na(out$combined_rate) & out$combined_rate > 0.20)
  out$share_with_outcomes <- out$with_outcomes / out$planned
  out$included <- out$share_with_outcomes >= 0.80
  out
}
