# Title: Analytic predictor of the divergence curve (candidate form of SPINAL; analysis plan, section 5.3)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model, on 2026-11-06 (step 5 of the work order of the coordination session
#   that followed the decision of the author of 2026-11-06 at 20:23). predict_spinal_levels()
#   repeats predict_levels() and e_ln_quadratic_form() of
#   time series R/SPINAL/validation-aci/checks/07b_walker_prediction.R at commit ced76be
#   (written by the orchestrating model, Claude Opus 5.5, on 2026-10-11), pinned unchanged in
#   inst/pinned/time_series_R/; the tests compare the two. window_slope_half_log_v() repeats
#   the slope of time series R/SPINAL/validation-aci/checks/10_ar1_against_fi.R (same commit).
#   walkbench 0.6.0 (2026-11-06): options of update 1 of the registration osf.io/m74tk for M2
#   (rules E1 to E5: maps given explicitly, odd and even separations, descriptive comparison by
#   the cumulated step durations alone), spinal_n_past(), spinal_settings_m2() and
#   spinal_lambda_l_lsq(); the defaults return the values of 07b. Option initial_rank_tol:
#   matching on the subspace that the input reaches, part of spinal_settings_m2() under update 2
#   of the registration (SIMACI/docs/osf/SIMACI_update_2.md, commit f5f3be8, approved on OSF on
#   2026-11-07).
#   walkbench 0.7.0 (2026-11-20, same session): help pages without the name of the
#   sub-project (decision 7 of the author of 2026-11-18) and the aliases predictor_levels(),
#   predictor_d_grid(), predictor_lambda_l_m1(), predictor_lambda_l_lsq(), predictor_n_past()
#   and predictor_settings_m2() (names chosen by the author on 2026-11-20); the former names
#   stay valid. The comments keep their references to the scripts of origin.
# Date: 2026-11-06
# Description: the candidate form of SPINAL (lots2_4_resultats.md, section 2.2) gives the
#   expected mean log divergence after k steps from the linearization of the step map
#   (linearize()) and the autocovariance of the input. In 07b the input is a sample-centered
#   FI(d) series; here the autocovariance is an argument, as section 5.3 of the analysis plan
#   requires (ARFIMA(0,d,0), AR(1), sample autocovariance of each Hausdorff series and of its
#   surrogates). With an FI(d) input, a centering over 620 values, 600 kept steps and the
#   linearization of M1, the function returns the values of 07b. The size of the reduced state
#   is read from the linearization; 07b fixes it at 2, the value of M1. No parameter is fitted
#   on simulated exponents.
# Dependencies: stats
# Input: none
# Output: none

#' Autocorrelation of FI(d) and AR(1) inputs
#'
#' `fi_autocorrelation()` uses the recursion rho_h = rho_(h-1) (h - 1 + d) / (h - d), the
#' product of `fi_rho_product()` of the certified equation EQ-18 of SIM-ARMA (Hosking 1981).
#'
#' @param d memory parameter.
#' @param phi coefficient of the AR(1) series.
#' @param n_lags number of lags, from lag 0.
#' @return The autocorrelations at lags 0 to `n_lags - 1`.
#' @export
fi_autocorrelation <- function(d, n_lags) {
  if (!is.finite(d) || d <= -1 || d >= 0.5) stop("d must satisfy -1 < d < 1/2")
  i <- seq_len(n_lags - 1L)
  c(1, cumprod((i - 1 + d) / (i - d)))
}

#' @rdname fi_autocorrelation
#' @export
ar1_autocorrelation <- function(phi, n_lags) {
  if (!is.finite(phi) || abs(phi) >= 1) stop("|phi| must be below 1")
  phi^(0:(n_lags - 1L))
}

#' Autocovariance of a series centered and rescaled over a window
#'
#' Approximation of 07b: (rho_h - vbar) / (1 - vbar) times `sd^2`, where vbar is the share of
#' the variance carried by the sample mean of `n_center` values.
#'
#' @param rho autocorrelations from lag 0; at least `n_center` lags.
#' @param n_center number of values over which the series is centered and rescaled.
#' @param sd standard deviation after rescaling.
#' @return A list with the autocovariance `gam` (same lags as `rho`) and `vbar`.
#' @export
centered_autocovariance <- function(rho, n_center, sd) {
  if (length(rho) < n_center) stop("rho needs at least n_center lags")
  h <- seq_len(n_center - 1L)
  vbar <- (n_center + 2 * sum((n_center - h) * rho[h + 1L])) / n_center^2
  list(gam = sd^2 * (rho - vbar) / (1 - vbar), vbar = vbar)
}

#' Sample autocovariance of a series
#'
#' @param x the series as the model receives it (for example after rescaling).
#' @param n_lags number of lags, from lag 0.
#' @return The autocovariance with the divisor n, at lags 0 to `n_lags - 1`.
#' @export
sample_autocovariance <- function(x, n_lags) {
  n <- length(x)
  if (n_lags > n) stop("n_lags exceeds the length of the series")
  y <- x - mean(x)
  vapply(0:(n_lags - 1L), function(h) sum(y[seq_len(n - h)] * y[(h + 1L):n]) / n, numeric(1))
}

#' Separations between epochs of the analytic predictor
#'
#' The name `predictor_d_grid()`, added in walkbench 0.7.0, gives the same function.
#'
#' @return The grid of separations, in steps, of 07b.
#' @export
spinal_d_grid <- function() c(2L, 4L, 6L, 8L, 10L, 14L, 20L, 30L, 50L, 80L, 120L, 200L, 300L,
                              400L, 500L)

# E ln(Z' Sigma Z) for Z standard normal, as in 07b (integral of statement A2 of SPINAL)
e_ln_quadratic_form <- function(sigma) {
  lam <- pmax(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values, 0)
  lam <- lam[lam > 1e-14 * max(lam)]
  sc <- max(lam)
  lam <- lam / sc
  f <- function(t) vapply(t, function(u) (exp(-u) - prod(1 + 2 * u * lam)^(-0.5)) / u, numeric(1))
  log(sc) + stats::integrate(f, 0, 1, rel.tol = 1e-7)$value +
    stats::integrate(f, 1, Inf, rel.tol = 1e-7)$value
}

#' Expected divergence levels of the analytic predictor
#'
#' Two epochs of one record, D steps apart, are matched at the start by equal post-contact
#' deviations. After k steps, their difference at local time l of the step is
#' M_p (-f(l) dTheta_k + Phi(l) dx_k), a Gaussian vector; the level is half the expected log
#' of its squared norm, averaged over the local time, the stance leg and D. The weights of D
#' are the number of pairs at that separation times the density at zero of the initial
#' difference. The name `predictor_levels()`, added in walkbench 0.7.0, gives the same
#' function.
#'
#' @param lin linearization of the model, from [linearize()].
#' @param gam autocovariance of the input from lag 0, one value per step; at least
#'   `n_past + max(k_levels) + max(d_grid)` lags.
#' @param n_steps number of steps of the record.
#' @param matched match the initial post-contact deviations (07b: `TRUE`).
#' @param k_levels numbers of steps at which the level is computed.
#' @param d_grid separations of the epochs, in steps; each even separation from 2 to
#'   `n_steps - 2` takes the nearest grid value.
#' @param n_past number of past values of the input in an epoch.
#' @param ell_step step of the subsample of local times.
#' @param maps list of the maps M_p from the state of the model to the coordinates of the
#'   estimation, one per stance leg. `NULL` (default) takes the maps of the linearization,
#'   the continuous coordinates of M1 (07b), and is allowed for M1 only; for M2 the maps are
#'   the identity (update 1 of the analysis plan, rule E1; [spinal_settings_m2()]).
#' @param separation_step step between the separations from 2 to `n_steps - 2`: 2 (07b, even
#'   separations) or 1 (M2, rule E2).
#' @param terms `"both"` (default): the difference has the term of the cumulated step
#'   durations and that of the states after contact. `"durations"`: the descriptive
#'   comparison of update 1, which keeps only the cumulated step durations.
#' @param initial_rank_tol `NULL` (default, 07b): the epochs are matched on the whole state
#'   after contact. A number: they are matched on the subspace that the input reaches, the
#'   left singular directions of the responses to the past inputs whose singular value
#'   exceeds `initial_rank_tol` times the largest; the density at zero of the initial
#'   difference is then taken on that subspace. In M2 the input reaches three of the four
#'   directions of the state after contact.
#' @return A list with `levels` (named by k), `weights` (normalized, by grid value), `period`
#'   and `initial_rank`, the dimension on which the epochs are matched.
#' @export
predict_spinal_levels <- function(lin, gam, n_steps, matched = TRUE, k_levels = 2:4,
                                  d_grid = spinal_d_grid(), n_past = 60L, ell_step = 5L,
                                  maps = NULL, separation_step = 2L,
                                  terms = c("both", "durations"), initial_rank_tol = NULL) {
  terms <- match.arg(terms)
  if (is.null(maps)) {
    if (!identical(lin$model, "M1")) {
      stop("give the maps explicitly for a model other than M1 (update 1 of the plan, rule E1)")
    }
    maps <- list(lin$m1, lin$m2)
  }
  # identical maps give identical terms: each distinct map is computed once, with its count
  umaps <- list()
  mult <- integer(0)
  for (m in maps) {
    hit <- which(vapply(umaps, identical, logical(1), m))
    if (length(hit) == 0L) {
      umaps[[length(umaps) + 1L]] <- m
      mult <- c(mult, 1L)
    } else {
      mult[hit] <- mult[hit] + 1L
    }
  }
  k_max <- max(k_levels)
  n_e <- n_past + k_max
  nx <- length(lin$b_vec)
  if (length(gam) < n_e + max(d_grid)) stop("gam needs more lags")
  p0 <- matrix(0, nx, n_e)
  for (j in seq_len(n_past)) {
    p0[, n_past - j + 1L] <- if (j == 1L) lin$b_vec else lin$j_map %*% p0[, n_past - j + 2L]
  }
  # directions of the state after contact on which the epochs are matched
  p0c <- p0
  if (!is.null(initial_rank_tol)) {
    sv <- svd(p0)
    rk <- sum(sv$d > initial_rank_tol * sv$d[1])
    p0c <- t(sv$u[, seq_len(rk), drop = FALSE]) %*% p0
  }
  p_k <- vector("list", k_max + 1L)
  q_k <- vector("list", k_max + 1L)
  p_k[[1L]] <- p0
  q_k[[1L]] <- matrix(0, 1L, n_e)
  for (k in seq_len(k_max)) {
    unit <- replace(numeric(n_e), n_past + k, 1)
    d_t <- lin$a_vec %*% p_k[[k]] + lin$e_sc * unit
    q_k[[k + 1L]] <- q_k[[k]] + d_t
    p_k[[k + 1L]] <- lin$j_map %*% p_k[[k]] + lin$b_vec %o% unit
  }
  ell_idx <- seq(1L, length(lin$ell) - 1L, by = ell_step)
  d_all <- seq(2L, n_steps - 2L, by = separation_step)
  # nearest node; which.min() takes the first minimum, the lower node in a tie
  near <- vapply(d_all, function(d) which.min(abs(d_grid - d)), integer(1))
  idx <- seq_len(n_e)
  lag0 <- abs(outer(idx, idx, "-"))
  out <- matrix(0, length(d_grid), length(k_levels))
  wts <- numeric(length(d_grid))
  for (gi in seq_along(d_grid)) {
    dsep <- d_grid[gi]
    cross <- matrix(gam[abs(outer(idx, idx, "-") - dsep) + 1L], n_e)
    omega <- 2 * matrix(gam[lag0 + 1L], n_e) - cross - t(cross)
    s0 <- p0c %*% omega %*% t(p0c)
    wts[gi] <- sum((n_steps - d_all[near == gi])) / sqrt(det(s0))
    om <- if (matched) omega - omega %*% t(p0c) %*% solve(s0, p0c %*% omega) else omega
    for (ki in seq_along(k_levels)) {
      kk <- k_levels[ki]
      r_k <- if (terms == "both") rbind(q_k[[kk + 1L]], p_k[[kk + 1L]]) else q_k[[kk + 1L]]
      s_k <- r_k %*% om %*% t(r_k)
      acc <- 0
      for (li in ell_idx) {
        a_mat <- if (terms == "both") {
          cbind(-lin$f_star[li, ], matrix(lin$phi[li, , ], ncol = nx))
        } else {
          matrix(-lin$f_star[li, ], ncol = 1L)
        }
        for (mi in seq_along(umaps)) {
          am <- umaps[[mi]] %*% a_mat
          acc <- acc + mult[mi] * 0.5 * e_ln_quadratic_form(am %*% s_k %*% t(am))
        }
      }
      out[gi, ki] <- acc / (length(maps) * length(ell_idx))
    }
  }
  wts <- wts / sum(wts)
  y <- colSums(out * wts)
  list(levels = stats::setNames(y, paste0("k=", k_levels)),
       weights = stats::setNames(wts, paste0("D=", d_grid)), period = lin$period,
       initial_rank = nrow(p0c))
}

#' Predicted long-term exponent of M1 from the levels of the analytic predictor
#'
#' (y(4 steps) - y(3 steps)) / T, per unit of model time, the quantity that M1 averages over
#' its window of 3 to 4 steps (07b). The name `predictor_lambda_l_m1()`, added in walkbench
#' 0.7.0, gives the same function.
#'
#' @param pred result of [predict_spinal_levels()] with levels at 3 and 4 steps.
#' @return The predicted lambda_L of M1.
#' @export
spinal_lambda_l_m1 <- function(pred) {
  unname(pred$levels[["k=4"]] - pred$levels[["k=3"]]) / pred$period
}

#' Number of past inputs of an epoch (update 1 of the analysis plan, rule E4)
#'
#' The smallest n for which `rho^n <= tol` and the Euclidean norm of J^n b over the state
#' variables is at most `tol` times that of b (J: step map; b: response of the state to the
#' input). The update states rho = 0.923, the largest eigenvalue modulus of M2 rounded to three
#' decimals. The name `predictor_n_past()`, added in walkbench 0.7.0, gives the same function.
#'
#' @param lin linearization of the model, from [linearize()].
#' @param rho modulus of the first condition.
#' @param tol tolerance of both conditions.
#' @param n_max largest number examined.
#' @return The number of past inputs, an integer.
#' @export
spinal_n_past <- function(lin, rho = 0.923, tol = 1e-4, n_max = 10000L) {
  n <- 1L
  while (rho^n > tol) n <- n + 1L
  b <- as.numeric(lin$b_vec)
  nb <- sqrt(sum(b^2))
  v <- b
  for (i in seq_len(n)) v <- as.numeric(lin$j_map %*% v)
  while (sqrt(sum(v^2)) > tol * nb) {
    n <- n + 1L
    if (n > n_max) stop("the norm condition does not hold up to n_max")
    v <- as.numeric(lin$j_map %*% v)
  }
  n
}

#' Settings of the predictor for M2 (update 1 of the analysis plan)
#'
#' Rules E1 to E4 of update 1 of the registration osf.io/m74tk: identity maps for both stance
#' legs; every separation from 2 to the length minus 2, odd and even, on the nodes 2 to 20, 30,
#' 50, 80, 120, 200, 300, 400 and 500 steps, with the nodes 700, 900 and 1,000 steps added for
#' records of 1,200 steps (section 3 of the update); levels at K = 8 to 20 steps; the number of
#' past inputs of [spinal_n_past()]. The grid without the added nodes is returned for the
#' sensitivity analysis. Update 2 of the registration (`SIMACI/docs/osf/SIMACI_update_2.md`, commit
#' f5f3be8) matches the epochs on the subspace that the input reaches, with the
#' relative threshold 1e-8 (`initial_rank_tol`); in M2 this subspace has three dimensions.
#' The name `predictor_settings_m2()`, added in walkbench 0.7.0, gives the same function.
#'
#' @param lin linearization of M2, from [linearize()].
#' @param n_steps number of steps of the record (600 or 1,200).
#' @return A list with `maps`, `separation_step`, `k_levels`, `n_past`, `initial_rank_tol`,
#'   `d_grid` (reference) and `d_grid_sensitivity`.
#' @export
spinal_settings_m2 <- function(lin, n_steps) {
  if (!identical(lin$model, "M2")) stop("the settings of update 1 are those of M2")
  nx <- length(lin$b_vec)
  grid <- c(2:20, 30L, 50L, 80L, 120L, 200L, 300L, 400L, 500L)
  grid_ref <- if (n_steps >= 1200L) c(grid, 700L, 900L, 1000L) else grid
  list(maps = list(diag(nx), diag(nx)), separation_step = 1L, k_levels = 8:20,
       n_past = spinal_n_past(lin), initial_rank_tol = 1e-8, d_grid = grid_ref,
       d_grid_sensitivity = grid)
}

#' Predicted long-term exponent from a least-squares slope of the levels
#'
#' The least-squares slope of y(K) on K, times `factor`. For M2 (update 1, rule E3), the
#' levels are K = 8 to 20 steps and the factor is 2, which gives lambda_L per stride. The name
#' `predictor_lambda_l_lsq()`, added in walkbench 0.7.0, gives the same function.
#'
#' @param pred result of [predict_spinal_levels()].
#' @param factor steps per unit of the exponent (2 for an exponent per stride).
#' @return The predicted lambda_L.
#' @export
spinal_lambda_l_lsq <- function(pred, factor = 2) {
  k <- as.numeric(sub("^k=", "", names(pred$levels)))
  factor * unname(stats::lm.fit(cbind(1, k), unname(pred$levels))$coefficients[2])
}

#' Slope of half the log variance of sums over a window
#'
#' v(K) is the variance of the sum of K consecutive values; the slope is the least-squares
#' slope of 0.5 ln v(K) on K over `k_window`, as in the script `10_ar1_against_fi.R` of the
#' repository "time series R" of the author.
#'
#' @param rho autocorrelations from lag 0, at least `max(k_window)` lags.
#' @param k_window values of K, in values of the series.
#' @return The slope per value of the series.
#' @export
window_slope_half_log_v <- function(rho, k_window) {
  y <- vapply(k_window, function(s) {
    h <- seq_len(s - 1L)
    0.5 * log(s * rho[1] + 2 * sum((s - h) * rho[h + 1L]))
  }, numeric(1))
  unname(stats::lm.fit(cbind(1, k_window), y)$coefficients[2])
}

#' FI level with the same window slope as an AR(1) series
#'
#' The value d_phi for which the slope of 0.5 ln v(K) over `k_window` equals that of the AR(1)
#' series (analysis plan, section 4.9); root of the difference on (-0.45, 0.49), as in the
#' reference scripts of the predictor (repository "time series R" of the author).
#'
#' @param phi coefficient of the AR(1) series.
#' @param k_window values of K, in steps (the plan: 4 to 8 steps).
#' @return d_phi.
#' @export
match_fi_to_ar1 <- function(phi, k_window) {
  n <- max(k_window) + 1L
  target <- window_slope_half_log_v(ar1_autocorrelation(phi, n), k_window)
  stats::uniroot(function(d) window_slope_half_log_v(fi_autocorrelation(d, n), k_window) - target,
                 c(-0.45, 0.49), tol = 1e-10)$root
}

# Aliases of walkbench 0.7.0: neutral names of the functions of the predictor, chosen by the
# author on 2026-11-20; the former names stay valid

#' @rdname predict_spinal_levels
#' @export
predictor_levels <- predict_spinal_levels

#' @rdname spinal_d_grid
#' @export
predictor_d_grid <- spinal_d_grid

#' @rdname spinal_lambda_l_m1
#' @export
predictor_lambda_l_m1 <- spinal_lambda_l_m1

#' @rdname spinal_lambda_l_lsq
#' @export
predictor_lambda_l_lsq <- spinal_lambda_l_lsq

#' @rdname spinal_n_past
#' @export
predictor_n_past <- spinal_n_past

#' @rdname spinal_settings_m2
#' @export
predictor_settings_m2 <- spinal_settings_m2
