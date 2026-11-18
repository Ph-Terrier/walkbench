# Title: Linearization of a model at its fixed point by central finite differences
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model; the computation follows the SPINAL script
#   checks/07a_walker_linearization.R of the repository "time series R" (written by the
#   orchestrating model, Claude Opus 5.5, on 2026-10-11), read only
# Date: 2026-11-05
# Description: x_(k+1) - x* = J (x_k - x*) + b u_k and T_k - T* = a'(x_k - x*) + e u_k,
#   plus the nominal state within the cycle, its time derivative and its sensitivities to
#   x and u (specification, section 6). The field names repeat those of
#   07a_walker_linearization.rds.
# Dependencies: none (the model supplies cycle_map(), flow() and rhs())
# Input: none
# Output: none

#' Linearize a model at its fixed point
#'
#' @param model a `simaci_model`.
#' @param injection injection point; it must be linearizable.
#' @param h step of the central finite differences.
#' @param n_grid number of local times from 0 to T* within the cycle.
#' @return A list with `x_star`, `period`, `j_map`, `b_vec`, `a_vec`, `e_sc`,
#'   `static_gain`, `eig`, `impulse_T` (lags 0 to 10), `ell`, `y_star`, `f_star`, `phi`,
#'   `psi`, `m1`, `m2`, `coord_maps`, `h`, `injection`, `model` and `produced_by`.
#' @export
linearize <- function(model, injection = model$default_injection, h = 1e-6, n_grid = 131L) {
  if (!isTRUE(model$injection_points[[injection]]$linearizable)) {
    stop("injection point without linearization: ", injection)
  }
  fp <- model$fixed_point()
  x_star <- fp$x_star
  period <- fp$period
  nx <- length(x_star)
  one <- function(x, u) {
    cm <- model$cycle_map(x, u, injection)
    c(cm$x_next, cm$duration)
  }
  jac <- matrix(0, nx + 1L, nx + 1L)
  for (j in seq_len(nx)) {
    dx <- replace(numeric(nx), j, h)
    jac[, j] <- (one(x_star + dx, 0) - one(x_star - dx, 0)) / (2 * h)
  }
  jac[, nx + 1L] <- (one(x_star, h) - one(x_star, -h)) / (2 * h)
  j_map <- jac[seq_len(nx), seq_len(nx), drop = FALSE]
  b_vec <- jac[seq_len(nx), nx + 1L]
  a_vec <- jac[nx + 1L, seq_len(nx)]
  e_sc <- jac[nx + 1L, nx + 1L]
  static_gain <- e_sc + sum(a_vec * solve(diag(nx) - j_map, b_vec))
  impulse <- numeric(11L)
  impulse[1] <- e_sc
  jp <- diag(nx)
  for (lag in 1:10) {
    impulse[lag + 1L] <- sum(a_vec * (jp %*% b_vec))
    jp <- jp %*% j_map
  }
  ell <- seq(0, period, length.out = n_grid)
  y_star <- model$flow(x_star, 0, ell, injection)
  f_star <- t(apply(y_star, 1, model$rhs))
  phi <- array(0, c(length(ell), ncol(y_star), nx))
  for (j in seq_len(nx)) {
    dx <- replace(numeric(nx), j, h)
    phi[, , j] <- (model$flow(x_star + dx, 0, ell, injection) -
                     model$flow(x_star - dx, 0, ell, injection)) / (2 * h)
  }
  psi <- (model$flow(x_star, h, ell, injection) - model$flow(x_star, -h, ell, injection)) /
    (2 * h)
  maps <- model$coord_maps
  list(x_star = x_star, period = period, j_map = j_map, b_vec = b_vec, a_vec = a_vec,
       e_sc = e_sc, static_gain = static_gain, eig = eigen(j_map)$values,
       impulse_T = impulse, ell = ell, y_star = y_star, f_star = f_star, phi = phi,
       psi = psi, m1 = maps[[1]], m2 = if (length(maps) > 1L) maps[[2]] else NULL,
       coord_maps = maps, h = h, injection = injection, model = model$id,
       produced_by = paste0("walkbench ", utils::packageVersion("walkbench"),
                            ", linearize(), written by the SP1.1 session (Claude Opus 5.5)"))
}
