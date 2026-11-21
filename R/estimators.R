# Title: Common estimators: divergence exponents, Floquet multipliers, variability, DFA
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model; estimate_divergence() generalizes the frozen
#   divergence_exponents() and floquet_profile() the frozen floquet_multipliers() of
#   feasibility/scripts/00_functions_walker.R, and both return the same values with the
#   settings of M1
# Date: 2026-11-05
# Description: specification, section 9. The divergence curve comes from the pinned RACING
#   engine (find_nearest_neighbors(), compute_divergence(), embed_time_series()), used
#   without modification; the initial separation, the smoothing, the slopes, the windows,
#   the resampling and the fixed pair set are code of this package.
#   walkbench 0.2.0 (2026-11-06): resampling "global_by_cycle" (the uniform grid of "global",
#   each grid point interpolated inside its own cycle) and estimation in the state variables of
#   the model referred to the stance leg (settings$coordinates = "model"), for the primary
#   settings of M2 of the analysis plan, draft 0.4 (table 5.1). The code repeats
#   resample_by_step() and to_model_vars() of SIMACI/wp1_replication/06_m2_estimation_sensitivity.R.
#   walkbench 0.4.0 (2026-11-06): DFA configuration of the frozen analysis plan (commit ba72afe,
#   section 5.2), that of Terrier (2026, Physica A 699:131842): DFA-1 with up to 30 box sizes
#   from 16 values to one third of the series, no alpha below 96 values. dfa_terrier2026()
#   repeats compute_dfa_alpha() of the Zenodo archive of that article ("time series R",
#   PhysicaA_paper/zenodo_archive_v2/01_scripts/functions/dfa.R) with a vectorized detrending;
#   the tests compare the two. The DFA of the feasibility study stays available for the replay.
#   walkbench 0.6.2 (2026-11-07): outcome_columns() names the outcome columns of the summary
#   row; estimate_all() returns them empty for a run that fell (review of the code of
#   campaigns 2 and 3 of M1, 2026-11-07, finding F4), so that runs.csv keeps its columns when
#   no simulation has outcomes.
#   walkbench 0.7.0 (2026-11-20, SP1.1 session, Claude Opus 5.5): the methods "global" and
#   "global_by_cycle" of resample_states() count their grid points with grid_count(), from the
#   number of cycles; the grid now always ends at the end of the run (decision of the author of
#   2026-11-19, finding of the web-simulator session of SIM-ACI, Claude Opus 5.5).
# Dependencies: stats, MASS; pinned RACING engine; frozen dfa_alpha()
# Input: none
# Output: none

# Number of points of the uniform grid of the methods "global" and "global_by_cycle": n_per
# points per unit of cycles_per_unit cycles, the start and the end of the run included. The
# cycles of a run are contiguous, so (t1 - t0) / mean_unit * n_per equals
# n_cycles * n_per / cycles_per_unit in exact arithmetic. Up to walkbench 0.6.2 the count was
# floor((t1 - t0) / mean_unit * n_per) + 1, computed from the times; a rounding error of about
# 1e-12 could put the argument of floor() just below the whole number, and the grid then lost
# its last point, at the end of the run. The count now comes from the number of cycles, which
# is exact and gives the same result on every platform (walkbench 0.7.0).
grid_count <- function(n_cycles, n_per, cycles_per_unit) {
  floor(n_cycles * n_per / cycles_per_unit) + 1L
}

# Linear resampling of the trajectory on a new time grid, with the exact cycle boundaries.
# `coords`: NULL (the sampled coordinates named in `state`), or the `model_coordinates` of the
# settings, whose maps turn the sampled coordinates of each cycle into the state variables of
# the model for the stance leg of that cycle; these jump at contact, so they need a resampling
# that interpolates inside each cycle
resample_states <- function(run, state, method, n_per, cycles_per_unit, coords = NULL) {
  smp <- run$samples
  ev <- run$events
  if (!is.null(coords) && method != "global_by_cycle") {
    stop("the state variables of the model need the resampling \"global_by_cycle\"")
  }
  if (method == "global_by_cycle") {
    # grid of "global"; grid point t belongs to the cycle k with t_start[k] <= t < t_end[k],
    # the last point of the run to the last cycle
    t0 <- ev$t_start[1]
    t1 <- ev$t_end[nrow(ev)]
    mean_unit <- mean(ev$duration) * cycles_per_unit
    n_out <- grid_count(nrow(ev), n_per, cycles_per_unit)
    grid <- pmin(t0 + (seq_len(n_out) - 1) * mean_unit / n_per, t1)
    k_of <- findInterval(grid, ev$t_start)
    src <- if (is.null(coords)) state else coords$sampled
    if (!is.null(coords) && !all(state %in% coords$names)) {
      stop("unknown state variables of the model: ", paste(setdiff(state, coords$names),
                                                           collapse = ", "))
    }
    X <- matrix(NA_real_, n_out, length(state))
    for (k in unique(k_of)) {
      gi <- which(k_of == k)
      idx <- which(smp[, "cycle"] == ev$cycle[k])
      tt <- c(ev$t_start[k], smp[idx, "t"], ev$t_end[k])
      yy <- rbind(run$bounds[[k]]$start[1, src], smp[idx, src, drop = FALSE],
                  run$bounds[[k]]$end[1, src])
      if (!is.null(coords)) {
        yy <- unname(yy) %*% t(coords$maps[[ev$leg[k]]])
        colnames(yy) <- coords$names
        yy <- yy[, state, drop = FALSE]
      }
      for (v in seq_along(state)) {
        X[gi, v] <- stats::approx(tt, yy[, v], xout = grid[gi], ties = mean)$y
      }
    }
    if (any(!is.finite(X))) stop("non-finite state after the resampling of each cycle")
    return(list(X = X, dt = 1 / n_per))
  }
  if (method == "global") {
    # uniform grid: n_per samples per unit on average (unit = cycles_per_unit cycles)
    t_all <- c(ev$t_start, smp[, "t"], ev$t_end)
    ord <- order(t_all)
    t0 <- ev$t_start[1]
    t1 <- ev$t_end[nrow(ev)]
    mean_unit <- mean(ev$duration) * cycles_per_unit
    n_out <- grid_count(nrow(ev), n_per, cycles_per_unit)
    # post-generation fix by Claude Opus 5.5, 2026-11-06: rounding could put the last grid
    # point just after t1, where approx() returns NA; the grid is now bounded by t1
    grid <- pmin(t0 + (seq_len(n_out) - 1) * mean_unit / n_per, t1)
    out <- vapply(seq_along(state), function(v) {
      y_all <- c(vapply(run$bounds, function(b) b$start[1, v], numeric(1)),
                 smp[, state[v]],
                 vapply(run$bounds, function(b) b$end[1, v], numeric(1)))
      stats::approx(t_all[ord], y_all[ord], xout = grid, ties = mean)$y
    }, numeric(length(grid)))
    return(list(X = matrix(out, ncol = length(state)), dt = 1 / n_per))
  }
  if (method == "per_cycle") {
    # each cycle to n_per samples, boundary states included once
    pieces <- lapply(seq_len(nrow(ev)), function(k) {
      idx <- which(smp[, "cycle"] == ev$cycle[k] & smp[, "t"] > ev$t_start[k] &
                     smp[, "t"] < ev$t_end[k])
      tt <- c(ev$t_start[k], smp[idx, "t"], ev$t_end[k])
      grid <- seq(ev$t_start[k], ev$t_end[k], length.out = n_per + 1L)[seq_len(n_per)]
      vapply(seq_along(state), function(v) {
        yy <- c(run$bounds[[k]]$start[1, v], smp[idx, state[v]], run$bounds[[k]]$end[1, v])
        stats::approx(tt, yy, xout = grid, ties = "ordered")$y
      }, numeric(n_per))
    })
    return(list(X = do.call(rbind, pieces), dt = 1 / (n_per * cycles_per_unit)))
  }
  stop("unknown resampling method: ", method)
}

# Divergence exponents of one state matrix
divergence_core <- function(X, dt, unit_len, settings, racing) {
  N <- nrow(X)
  n_hor <- round(settings$horizon * unit_len / dt)
  theiler <- round(settings$theiler * unit_len / dt)
  nbr <- racing$find_nearest_neighbors(X, mean_period = theiler)
  full <- seq_len(N) <= N - n_hor & nbr <= N - n_hor
  if (identical(settings$pairs, "fixed")) {
    nbr[!full] <- N + 1L
    d0 <- sqrt(rowSums((X[full, , drop = FALSE] - X[nbr[full], , drop = FALSE])^2))
  } else {
    d0 <- sqrt(rowSums((X - X[nbr, , drop = FALSE])^2))
  }
  mld_k <- racing$compute_divergence(X, nbr, max_iter = n_hor)
  mld <- c(mean(log(d0[d0 > 0])), mld_k)  # mld[i + 1] = after i samples
  if (identical(settings$smoothing, "hamming7")) {
    wts <- 0.54 - 0.46 * cos(2 * pi * (0:6) / 6)
    wts <- wts / sum(wts)
    mld_f <- as.numeric(stats::filter(mld, wts, sides = 2))
  } else {
    mld_f <- mld
  }
  tt <- (0:n_hor) * dt
  in_s <- tt >= settings$window_s[1] * unit_len & tt <= settings$window_s[2] * unit_len
  in_l <- tt >= settings$window_l[1] * unit_len & tt <= settings$window_l[2] * unit_len
  slopes <- rep(NA_real_, n_hor + 1L)
  if (identical(settings$slope, "mean_local")) {
    for (i in 2:n_hor) slopes[i] <- (mld_f[i + 1] - mld_f[i - 1]) / (2 * dt)
    i_short <- which(!is.na(slopes) & tt <= settings$window_s[2] * unit_len &
                       tt >= settings$window_s[1] * unit_len)
    i_long <- which(!is.na(slopes) & tt >= settings$window_l[1] * unit_len &
                      tt <= settings$window_l[2] * unit_len)
    lambda_s <- mean(slopes[i_short])
    lambda_l <- mean(slopes[i_long])
  } else if (identical(settings$slope, "lsq")) {
    lsq <- function(sel) {
      sel <- sel & is.finite(mld_f)
      unname(stats::lm.fit(cbind(1, tt[sel]), mld_f[sel])$coefficients[2])
    }
    lambda_s <- lsq(in_s)
    lambda_l <- lsq(in_l)
  } else {
    stop("unknown slope method: ", settings$slope)
  }
  list(mld = mld, mld_smooth = mld_f, slopes = slopes, time = tt, lambda_s = lambda_s,
       lambda_l = lambda_l, n_pairs = sum(full), neighbors = nbr, n_horizon = n_hor,
       theiler = theiler)
}

#' Short-term and long-term divergence exponents
#'
#' Steps of section 9.2 of the specification: coordinates or delay embedding, optional
#' resampling, nearest neighbors and mean logarithmic divergence by the pinned RACING
#' engine, initial separation, pair set, slope over the windows of lambda_S and lambda_L.
#'
#' With `settings$coordinates = "model"`, the coordinates are the state variables of the
#' model referred to the stance leg. They are recovered from the sampled coordinates of each
#' cycle with the maps of `settings$model_coordinates`, and the resampling must be
#' `"global_by_cycle"`: the uniform grid of the global resampling, with each grid point
#' interpolated inside its own cycle ([settings_m2()]).
#'
#' @param run a trimmed `simaci_run`.
#' @param settings estimation settings, for example `model_m1()$estimation_settings`.
#' @param cycles_per_stride number of cycles of the model in one stride.
#' @return A list with `lambda_s`, `lambda_l` (native unit), `lambda_s_per_step`,
#'   `lambda_l_per_step`, `unit`, the curve (`mld`, `mld_smooth`, `slopes`, `time`) and
#'   `n_pairs`. With a delay embedding of several coordinates, a list of such lists.
#' @export
estimate_divergence <- function(run, settings, cycles_per_stride = 2L) {
  racing <- load_racing()
  method <- settings$resample$method %||% "none"
  coords <- NULL
  if (identical(settings$coordinates, "model")) {
    coords <- settings$model_coordinates
    if (is.null(coords$maps) || is.null(coords$names) || is.null(coords$sampled)) {
      stop("settings$model_coordinates must give names, sampled and maps")
    }
    if (method == "none") stop("the state variables of the model need a resampling")
  }
  durations <- run$events$duration
  cycle_len <- mean(durations)
  # cycles of the model per window unit
  cyc_per_unit <- if (identical(settings$window_unit, "stride")) cycles_per_stride else 1
  if (method == "none") {
    X <- unname(as.matrix(run$samples[, settings$state, drop = FALSE]))
    dt <- run$dt
    unit_len <- if (cyc_per_unit == 1) cycle_len else cyc_per_unit * cycle_len
    step_len <- cycle_len * if (cycles_per_stride == 2L) 1 else 0.5
    unit_name <- "per unit of model time"
  } else {
    per <- settings$resample$per %||% settings$window_unit
    per_cycles <- if (identical(per, "stride")) cycles_per_stride else 1
    rs <- resample_states(run, settings$state, method, settings$resample$n_per, per_cycles,
                          coords = coords)
    X <- rs$X
    dt <- rs$dt
    unit_len <- cyc_per_unit / per_cycles
    step_len <- (if (cycles_per_stride == 2L) 1 else 0.5) / per_cycles
    unit_name <- paste("per", per)
  }
  finish <- function(res) {
    res$lambda_s_per_step <- res$lambda_s * step_len
    res$lambda_l_per_step <- res$lambda_l * step_len
    res$unit <- unit_name
    res
  }
  if (is.null(settings$embedding)) return(finish(divergence_core(X, dt, unit_len, settings, racing)))
  emb <- settings$embedding
  out <- lapply(seq_len(ncol(X)), function(v) {
    Y <- racing$embed_time_series(X[, v], emb$m, emb$tau)
    finish(divergence_core(Y, dt, unit_len, settings, racing))
  })
  names(out) <- settings$state
  out
}

# Largest Floquet multiplier at each Poincare section of the normalized cycle; same method
# as the frozen floquet_multipliers(), for any number of state variables
floquet_profile <- function(arr, xstar, sections = seq_len(dim(arr)[2])) {
  n_str <- dim(arr)[1]
  n_var <- dim(arr)[3]
  vapply(sections, function(sec) {
    X <- arr[, sec, ] - matrix(xstar[sec, ], n_str, n_var, byrow = TRUE)
    Z <- X[-n_str, , drop = FALSE]
    Y <- X[-1, , drop = FALSE]
    B <- tryCatch(MASS::ginv(Z) %*% Y, error = function(e) NULL)
    if (is.null(B)) return(NA_real_)
    max(Mod(eigen(t(B), only.values = TRUE)$values))
  }, numeric(1))
}

#' Floquet multipliers from Poincare sections of the normalized strides
#'
#' @param run a trimmed `simaci_run`.
#' @param model the `simaci_model` that produced the run.
#' @param xstar mean normalized stride of an unperturbed reference run
#'   ([reference_xstar()]).
#' @param sections indices of the Poincare sections.
#' @return The largest eigenvalue modulus at each section.
#' @export
estimate_floquet <- function(run, model, xstar, sections = NULL) {
  arr <- model$normalize_cycles(run)
  floquet_profile(arr, xstar, sections %||% seq_len(dim(arr)[2]))
}

#' Mean normalized stride of a reference run
#'
#' @param run a trimmed, unperturbed `simaci_run`.
#' @param model the `simaci_model` that produced the run.
#' @return The matrix x* (sections x state variables).
#' @export
reference_xstar <- function(run, model) {
  apply(model$normalize_cycles(run), c(2, 3), mean)
}

# MeanSD: SD across strides at each normalized time, averaged over the stride (same code
# as the frozen mean_sd())
mean_sd_arr <- function(arr) {
  apply(arr, 3, function(m) mean(apply(m, 2, stats::sd)))
}

#' Kinematic variability and variability of the cycle durations
#'
#' @param run a trimmed `simaci_run`.
#' @param model the `simaci_model` that produced the run.
#' @return A list with `msd` (MeanSD of each coordinate), `mean_duration`, `sd_duration`
#'   and `cv_duration`.
#' @export
estimate_variability <- function(run, model) {
  d <- run$events$duration
  list(msd = mean_sd_arr(model$normalize_cycles(run)), mean_duration = mean(d),
       sd_duration = stats::sd(d), cv_duration = stats::sd(d) / mean(d))
}

#' Settings of the DFA
#'
#' `"terrier2026"` (default) is the configuration of the SIM-ACI analysis plan (frozen at
#' commit ba72afe, section 5.2), that of Terrier (2026, Physica A 699:131842): DFA-1, up to 30
#' box sizes spaced on a log scale from 16 values to one third of the series, no alpha below 96
#' values; the DFA of the input uses the kept part of the input. `"feasibility"` is the frozen
#' `dfa_alpha()` of the feasibility study: DFA-1, up to 20 window sizes from 4 to N/4, no alpha
#' below 16 values, on the whole input; the replay of the feasibility study uses it.
#'
#' @param method `"terrier2026"` or `"feasibility"`.
#' @return A list of class `simaci_dfa`.
#' @export
dfa_settings <- function(method = c("terrier2026", "feasibility")) {
  method <- match.arg(method)
  s <- if (method == "terrier2026") {
    list(method = method, n_min = 16, n_max_ratio = 1 / 3, n_sizes = 30L, order = 1L,
         min_length = 96L, input_part = "kept")
  } else {
    list(method = method, n_min = 4, n_max_ratio = 1 / 4, n_sizes = 20L, order = 1L,
         min_length = 16L, input_part = "whole")
  }
  structure(s, class = "simaci_dfa")
}

# DFA of Terrier (2026): repeats compute_dfa_alpha() of the Zenodo archive of the article
# (box sizes, profile, detrending by least squares in non-overlapping boxes from the start of
# the series, F(n) = root mean square of the residuals, slope of log10 F on log10 n), with the
# detrending of all boxes of one size in one matrix product
dfa_terrier2026 <- function(x, n_min = 16, n_max_ratio = 1 / 3, n_sizes = 30L, order = 1L) {
  n_x <- length(x)
  n_max <- floor(n_x * n_max_ratio)
  if (n_max < n_min + 2) return(NA_real_)
  sizes <- unique(round(n_min * (n_max / n_min)^(seq(0, 1, length.out = n_sizes))))
  sizes <- sizes[sizes >= n_min & sizes <= n_max]
  if (length(sizes) < 5L) return(NA_real_)
  y <- cumsum(x - mean(x))
  fn <- vapply(sizes, function(n) {
    n_box <- floor(n_x / n)
    yy <- matrix(y[seq_len(n_box * n)], nrow = n)
    xx <- outer(seq_len(n), 0:order, `^`)
    res <- yy - xx %*% qr.solve(xx, yy)
    sqrt(mean(res^2))
  }, numeric(1))
  ok <- fn > 0
  if (sum(ok) < 5L) return(NA_real_)
  unname(stats::lm.fit(cbind(1, log10(sizes[ok])), log10(fn[ok]))$coefficients[2])
}

#' DFA alpha of a series
#'
#' @param x numeric series.
#' @param dfa settings from [dfa_settings()]; by default the configuration of the analysis
#'   plan.
#' @return The scaling exponent, or `NA` for a series that is too short, constant or not
#'   finite.
#' @export
estimate_dfa <- function(x, dfa = dfa_settings()) {
  if (!inherits(dfa, "simaci_dfa")) stop("dfa must come from dfa_settings()")
  if (length(x) < dfa$min_length || any(!is.finite(x)) || stats::sd(x) == 0) return(NA_real_)
  if (dfa$method == "feasibility") return(m1_walker_env()$dfa_alpha(x)$alpha)
  dfa_terrier2026(x, dfa$n_min, dfa$n_max_ratio, dfa$n_sizes, dfa$order)
}

# Outcome columns of the summary row of estimate_all(), in their order (walkbench 0.6.2)
outcome_columns <- function(model) {
  c("mean_duration", "sd_duration", "cv_duration",
    paste0("msd_", model$state_names, recycle0 = TRUE),
    "fm25", "fm50", "fm75", "fm_max", "lambda_s", "lambda_l", "lambda_s_per_step",
    "lambda_l_per_step", "n_pairs", "alpha_input", "alpha_command", "alpha_duration",
    "alpha_stride_duration", "alpha_asynchrony")
}

# The outcome columns, empty, for a run or a trial without outcomes
outcomes_na <- function(model) {
  nm <- outcome_columns(model)
  as.data.frame(stats::setNames(as.list(rep(NA_real_, length(nm))), nm), check.names = FALSE)
}

#' All outcomes of one run, as one summary row
#'
#' @param run a trimmed `simaci_run`.
#' @param model the `simaci_model` that produced the run.
#' @param settings estimation settings.
#' @param xstar reference stride for the Floquet multipliers, or `NULL`.
#' @param input the input of the run, for the DFA of the generated series.
#' @param cell,replicate identifiers written in the row.
#' @param dfa settings of the DFA, from [dfa_settings()]. With the default configuration of
#'   the analysis plan, the DFA of the input uses its kept part (the values of the kept
#'   cycles); with `dfa_settings("feasibility")` it uses the whole input, as in the
#'   feasibility study.
#' @return A one-row data frame (specification, section 8.2), with the divergence curve in
#'   the attribute `"divergence"`. `alpha_stride_duration` is the DFA alpha of the stride
#'   durations, the sums of the kept step durations taken in pairs from the first kept step.
#'   For a run that fell, the outcome columns are present and empty (walkbench 0.6.2), and
#'   the row has no divergence curve.
#' @export
estimate_all <- function(run, model, settings = model$estimation_settings, xstar = NULL,
                         input = run$input, cell = NA, replicate = NA, dfa = dfa_settings()) {
  row <- data.frame(model = model$id, injection = run$injection, cell = cell,
                    replicate = replicate, n_redraws = run$n_redraws %||% NA_integer_,
                    fell = run$fell)
  if (run$fell) return(cbind(row, outcomes_na(model)))
  arr <- model$normalize_cycles(run)
  msd <- mean_sd_arr(arr)
  d <- run$events$duration
  fm <- if (is.null(xstar)) rep(NA_real_, dim(arr)[2]) else floquet_profile(arr, xstar)
  dv <- estimate_divergence(run, settings, model$cycles_per_stride %||% 2L)
  asy <- run$events$asynchrony
  cps <- model$cycles_per_stride %||% 2L
  n_str <- length(d) %/% cps
  strides <- colSums(matrix(d[seq_len(n_str * cps)], nrow = cps))
  input_dfa <- if (is.null(input)) NULL else if (dfa$input_part == "kept") {
    input$values[(run$n_transient %||% 0L) + seq_len(run$n_cycles_done)]
  } else {
    input$values
  }
  out <- data.frame(
    mean_duration = mean(d), sd_duration = stats::sd(d), cv_duration = stats::sd(d) / mean(d),
    t(stats::setNames(msd, paste0("msd_", model$state_names))),
    fm25 = fm[26], fm50 = fm[51], fm75 = fm[76],
    fm_max = if (all(is.na(fm))) NA_real_ else max(fm, na.rm = TRUE),
    lambda_s = dv$lambda_s, lambda_l = dv$lambda_l,
    lambda_s_per_step = dv$lambda_s_per_step, lambda_l_per_step = dv$lambda_l_per_step,
    n_pairs = dv$n_pairs,
    alpha_input = if (is.null(input_dfa)) NA_real_ else estimate_dfa(input_dfa, dfa),
    alpha_command = estimate_dfa(run$events$u, dfa),
    alpha_duration = estimate_dfa(d, dfa),
    alpha_stride_duration = if (n_str < 1L) NA_real_ else estimate_dfa(strides, dfa),
    alpha_asynchrony = if (all(is.na(asy))) NA_real_ else estimate_dfa(asy, dfa))
  # the row of a run that fell takes its columns from outcome_columns()
  if (!identical(names(out), outcome_columns(model))) {
    stop("estimate_all(): the outcome columns differ from outcome_columns()")
  }
  row <- cbind(row, out)
  attr(row, "divergence") <- dv
  row
}
