# Title: Prediction of the long-term divergence of the SIM-ACI walker from its linearization
#   (lot 4, stage 2)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the orchestrating model, Claude Opus 5.5, without a code-generation
#   model
# Date: 2026-10-11
# Description: lot 4 of the work plan SPINAL/validation-aci/plan_validation_mathematique_aci.md
#   (statements B7 and B8). Candidate form drafted by the orchestrating model; no parameter is
#   fitted to the archived results.
#   Linear model of one epoch: dx_(k+1) = J dx_k + b eps_k, dT_k = a' dx_k + e eps_k, with dx the
#   deviation of the post-heel-strike state and dT the deviation of the step duration. After K
#   steps, a point at local time l of its step differs between two epochs by
#     M_p [ -f(l) dTheta_K + Phi(l) dx_K ],
#   where dTheta_K is the difference of the cumulated step durations, dx_K the difference of the
#   post-heel-strike deviations, f the state velocity on the cycle, Phi the sensitivity of the
#   state to the post-heel-strike state and M_p the map to continuous coordinates (stance leg p).
#   The two epochs belong to one record and are D steps apart (D even, at least 2). They are
#   matched at the start: equal post-heel-strike deviations (limit of an infinite pool of
#   neighbors). The difference vector is normal; its mean log norm is 0.5 E ln(Z' C Z)
#   (integral of statement A2). The curve averages over the local time, the stance leg and D,
#   with weights (number of pairs at separation D) x (density at zero of the initial difference).
#   Input: the perturbation is a sample-centered FI(d) series of 620 steps scaled to a fixed
#   standard deviation, as in the feasibility study; its autocovariance is approximated by
#   (rho_h - vbar) / (1 - vbar), vbar being the variance of the sample mean.
#   Predicted long-term exponent: [y(4 T*) - y(3 T*)] / T*, per unit of model time, the
#   quantity that the study averages between 3 and 4 steps.
#   The script predicts divergence levels and exponents only. It states nothing on the model
#   class of the step durations.
#   Published equations go through the certified library of SIM-ARMA/equations (EQ-18).
# Dependencies: stats; yaml and tools through the certified library
# Input: R/functions/certified_equations.R (run from the SIM-ARMA folder);
#   ../SPINAL/validation-aci/checks/07a_walker_linearization.rds
# Output: ../SPINAL/validation-aci/checks/07b_walker_prediction_results.txt
# Usage (from the repository root):
#   cd SIM-ARMA
#   Rscript ../SPINAL/validation-aci/checks/07b_walker_prediction.R

suppressPackageStartupMessages(library(stats))
source("R/functions/certified_equations.R")

chk <- "../SPINAL/validation-aci/checks/"
out_txt <- paste0(chk, "07b_walker_prediction_results.txt")
if (file.exists(out_txt)) {
  invisible(file.remove(out_txt))
}
say <- function(...) {
  line <- sprintf(...)
  cat(line, "\n", sep = "")
  cat(line, "\n", sep = "", file = out_txt, append = TRUE)
}
lin <- readRDS(paste0(chk, "07a_walker_linearization.rds"))

fi_acf <- function(d, n_lags) {
  if (d == 0) {
    return(c(1, rep(0, n_lags - 1)))
  }
  g <- eq18_fi_acvf(d, n_lags)
  g / g[1]
}
e_ln_quadratic_form <- function(sigma) {
  lam <- pmax(eigen(sigma, symmetric = TRUE, only.values = TRUE)$values, 0)
  lam <- lam[lam > 1e-14 * max(lam)]
  sc <- max(lam)
  lam <- lam / sc
  f <- function(t) vapply(t, function(u) (exp(-u) - prod(1 + 2 * u * lam)^(-0.5)) / u, numeric(1))
  log(sc) + integrate(f, 0, 1, rel.tol = 1e-7)$value + integrate(f, 1, Inf, rel.tol = 1e-7)$value
}

n_past <- 60L
k_max <- 4L
n_e <- n_past + k_max
n_series <- 620L
n_steps <- lin$n_steps
# Linear maps from the perturbation vector (eps_-60 .. eps_-1, eps_0 .. eps_3) of one epoch
p_k <- vector("list", k_max + 1L)
q_k <- vector("list", k_max + 1L)
p0 <- matrix(0, 2L, n_e)
for (j in seq_len(n_past)) {
  p0[, n_past - j + 1L] <- if (j == 1L) lin$b_vec else lin$j_map %*% p0[, n_past - j + 2L]
}
p_k[[1L]] <- p0
q_k[[1L]] <- matrix(0, 1L, n_e)
for (k in seq_len(k_max)) {
  unit <- replace(numeric(n_e), n_past + k, 1)
  d_t <- lin$a_vec %*% p_k[[k]] + lin$e_sc * unit
  q_k[[k + 1L]] <- q_k[[k]] + d_t
  p_k[[k + 1L]] <- lin$j_map %*% p_k[[k]] + lin$b_vec %o% unit
}

ell_idx <- seq(1L, length(lin$ell) - 1L, by = 5L)
d_grid <- c(2L, 4L, 6L, 8L, 10L, 14L, 20L, 30L, 50L, 80L, 120L, 200L, 300L, 400L, 500L)
d_all <- seq(2L, n_steps - 2L, by = 2L)
near <- vapply(d_all, function(d) which.min(abs(d_grid - d)), integer(1))

predict_levels <- function(d, matched = TRUE) {
  rho <- fi_acf(d, n_series + 700L)
  h <- seq_len(n_series - 1L)
  vbar <- (n_series + 2 * sum((n_series - h) * rho[h + 1L])) / n_series^2
  gam <- lin$sd_eps^2 * (rho - vbar) / (1 - vbar)
  idx <- seq_len(n_e)
  lag0 <- abs(outer(idx, idx, "-"))
  out <- matrix(0, length(d_grid), 3L)
  wts <- numeric(length(d_grid))
  for (gi in seq_along(d_grid)) {
    dsep <- d_grid[gi]
    cross <- matrix(gam[abs(outer(idx, idx, "-") - dsep) + 1L], n_e)
    omega <- 2 * matrix(gam[lag0 + 1L], n_e) - cross - t(cross)
    s0 <- p0 %*% omega %*% t(p0)
    wts[gi] <- sum((n_steps - d_all[near == gi])) / sqrt(det(s0))
    om <- if (matched) omega - omega %*% t(p0) %*% solve(s0, p0 %*% omega) else omega
    for (kk in 2:4) {
      r_k <- rbind(q_k[[kk + 1L]], p_k[[kk + 1L]])
      s_k <- r_k %*% om %*% t(r_k)
      acc <- 0
      for (li in ell_idx) {
        a_mat <- cbind(-lin$f_star[li, ], lin$phi[li, , 1L], lin$phi[li, , 2L])
        for (m_p in list(lin$m1, lin$m2)) {
          am <- m_p %*% a_mat
          acc <- acc + 0.5 * e_ln_quadratic_form(am %*% s_k %*% t(am))
        }
      }
      out[gi, kk - 1L] <- acc / (2 * length(ell_idx))
    }
  }
  wts <- wts / sum(wts)
  y <- colSums(out * wts)
  c(y2 = y[1], y3 = y[2], y4 = y[3], lambda_l = (y[3] - y[2]) / lin$period, vbar = vbar)
}

say("Prediction of the long-term divergence of the SIM-ACI walker from its linearization (lot 4)")
say("Produced by: script 07b_walker_prediction.R, written by the orchestrating model, Claude Opus 5.5")
say("Run on %s (machine clock), %s", format(Sys.time(), "%Y-%m-%d %H:%M"), R.version.string)
say("Status: candidate form proposed by the orchestrating model; no parameter fitted to the archived results")
say("Linearization: period %.4f, step-map eigenvalue modulus %.4f, perturbation sd %.4f",
    lin$period, Mod(eigen(lin$j_map)$values[1]), lin$sd_eps)
say("")
say("columns: d | variance share of the sample mean | predicted level at 2, 3, 4 steps | predicted lambda_L | archived mean lambda_L | archived sd over 10 trials | (predicted - archived) / standard error of the archived mean")
tg <- lin$targets
pred <- t(vapply(tg$d, predict_levels, numeric(5)))
for (i in seq_len(nrow(tg))) {
  say("  d = %+.2f | %.3f | %7.3f %7.3f %7.3f | %.4f | %.4f | %.4f | %6.2f",
      tg$d[i], pred[i, "vbar"], pred[i, "y2"], pred[i, "y3"], pred[i, "y4"], pred[i, "lambda_l"],
      tg$lambda_l[i], tg$lambda_l_sd[i], (pred[i, "lambda_l"] - tg$lambda_l[i]) / (tg$lambda_l_sd[i] / sqrt(tg$n_trials[i])))
}
say("")
say("correlation between predicted and archived mean lambda_L over the 9 levels: %.3f", cor(pred[, "lambda_l"], tg$lambda_l))
fit <- lm(tg$lambda_l ~ pred[, "lambda_l"])
say("regression of archived on predicted: intercept %.4f, slope %.3f", coef(fit)[1], coef(fit)[2])
pos <- tg$d >= 0
say("slope of the predicted level against d, levels d >= 0: at 2 steps %.3f, at 4 steps %.3f (guide, chapter 9: -0.31 and +0.31)",
    coef(lm(pred[pos, "y2"] ~ tg$d[pos]))[2], coef(lm(pred[pos, "y4"] ~ tg$d[pos]))[2])
say("")
say("Same prediction without the matching of the initial post-heel-strike deviations:")
pred_f <- t(vapply(tg$d, predict_levels, numeric(5), matched = FALSE))
for (i in seq_len(nrow(tg))) {
  say("  d = %+.2f | predicted lambda_L %.4f | archived %.4f", tg$d[i], pred_f[i, "lambda_l"], tg$lambda_l[i])
}
say("")
say("End.")
