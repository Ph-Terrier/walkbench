# Title: Closed-loop port: metronome, controllers of H3a and H3b, gain scales, linear stability
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: a controller is called once at each contact, before cycle k starts, with the
#   state and the time of contact k - 1 (specification, section 5). The gain scales and the
#   linearized closed loops follow section 7 of the specification; the scales "deadbeat"
#   (H3a) and "static" (H3b) were approved by the author on 2026-11-05 (open choice O1).
# Dependencies: none
# Input: none
# Output: none

#' Metronome of constant period
#'
#' @param period period P of the beats, in the time unit of the model; beat j occurs at
#'   j * P, from the start of the simulation.
#' @return A list of class `simaci_metronome`.
#' @export
make_metronome <- function(period) {
  force(period)
  structure(list(period = period, beat = function(j) j * period),
            class = "simaci_metronome")
}

#' Controller of variant H3a, correction of the state
#'
#' Input of cycle k: `u_k = w_k - g * corr_k`. Scale `"shift"`: `corr_k = delta_k / (c'b)`,
#' where `delta_k` is the deviation of the corrected coordinate at the contact that starts
#' cycle k. Scale `"deadbeat"`: `corr_k = c'J (x_k - x*) / (c'b)`; a gain of 1 cancels, to
#' first order, the predicted deviation of that coordinate at the next contact.
#'
#' @param w exogenous series, a `simaci_input`.
#' @param g gain.
#' @param lin linearization of the model, from [linearize()].
#' @param scale `"deadbeat"` (default) or `"shift"`.
#' @param corrected_index index of the corrected coordinate in the reduced state.
#' @return A controller function `function(k, info)`.
#' @export
make_controller_h3a <- function(w, g, lin, scale = c("deadbeat", "shift"),
                                corrected_index = 1L) {
  scale <- match.arg(scale)
  wv <- w$values
  x_star <- lin$x_star
  ci <- corrected_index
  cb <- lin$b_vec[ci]
  cj <- lin$j_map[ci, ]
  function(k, info) {
    dx <- info$x_post - x_star
    delta <- dx[ci]
    corr <- if (scale == "shift") g * delta / cb else g * sum(cj * dx) / cb
    list(u = wv[k] - corr, u_exo = wv[k], u_corr = -corr, deviation = delta)
  }
}

#' Controller of variant H3b, correction of the asynchrony with a metronome
#'
#' Input of cycle k: `u_k = c_k - kappa * s * A_(k-1)`, with `s = 1/G` (scale `"static"`,
#' default) or `s = 1/e` (scale `"immediate"`). Scale `"full_state"`:
#' `u_k = c_k - kappa * (A_(k-1) + a'(x_k - x*)) / e`. The run needs a metronome.
#'
#' @param c_series timekeeper series, a `simaci_input`.
#' @param kappa gain.
#' @param lin linearization of the model, from [linearize()].
#' @param scale `"static"`, `"immediate"` or `"full_state"`.
#' @return A controller function `function(k, info)`.
#' @export
make_controller_h3b <- function(c_series, kappa, lin,
                                scale = c("static", "immediate", "full_state")) {
  scale <- match.arg(scale)
  cv <- c_series$values
  s <- if (scale == "static") 1 / lin$static_gain else 1 / lin$e_sc
  a_vec <- lin$a_vec
  x_star <- lin$x_star
  function(k, info) {
    a_prev <- info$asynchrony
    if (is.na(a_prev)) stop("the controller of H3b needs a metronome")
    corr <- if (scale == "full_state") {
      kappa * (a_prev + sum(a_vec * (info$x_post - x_star))) * s
    } else {
      kappa * s * a_prev
    }
    list(u = cv[k] - corr, u_exo = cv[k], u_corr = -corr, deviation = a_prev)
  }
}

#' Gain scale of a controller
#'
#' @param lin linearization of the model, from [linearize()].
#' @param controller `"h3a"` or `"h3b"`.
#' @param scale for H3a, `"shift"` or `"deadbeat"`; for H3b, `"immediate"`, `"static"` or
#'   `"full_state"`.
#' @param corrected_index index of the corrected coordinate (H3a).
#' @return For the scales `"shift"`, `"immediate"`, `"static"` and `"full_state"`, the
#'   factor s; for `"deadbeat"`, the row vector c'J / (c'b).
#' @export
gain_scale <- function(lin, controller = c("h3a", "h3b"), scale, corrected_index = 1L) {
  controller <- match.arg(controller)
  ci <- corrected_index
  if (controller == "h3a") {
    switch(scale, shift = 1 / lin$b_vec[ci],
           deadbeat = lin$j_map[ci, ] / lin$b_vec[ci],
           stop("unknown scale of H3a: ", scale))
  } else {
    switch(scale, immediate = 1 / lin$e_sc, static = 1 / lin$static_gain,
           full_state = 1 / lin$e_sc, stop("unknown scale of H3b: ", scale))
  }
}

#' Matrix of the linearized closed loop
#'
#' H3a: state x_k - x*. H3b: state (x_k - x*, A_(k-1)).
#'
#' @inheritParams gain_scale
#' @param gain gain g (H3a) or kappa (H3b).
#' @return The transition matrix of the linearized closed loop.
#' @export
closed_loop_matrix <- function(lin, controller = c("h3a", "h3b"), scale, gain,
                               corrected_index = 1L) {
  controller <- match.arg(controller)
  jm <- lin$j_map
  b <- lin$b_vec
  a <- lin$a_vec
  e <- lin$e_sc
  nx <- length(b)
  if (controller == "h3a") {
    cvec <- replace(numeric(nx), corrected_index, 1)
    if (scale == "shift") {
      return(jm - gain * (b %*% t(cvec)) / b[corrected_index])
    }
    if (scale == "deadbeat") {
      return(jm - gain * (b %*% t(jm[corrected_index, ])) / b[corrected_index])
    }
    stop("unknown scale of H3a: ", scale)
  }
  if (scale == "full_state") {
    return(rbind(cbind(jm - gain * (b %*% t(a)) / e, -gain * b / e),
                 c((1 - gain) * a, 1 - gain)))
  }
  s <- gain_scale(lin, "h3b", scale)
  rbind(cbind(jm, -gain * s * b), c(a, 1 - gain * s * e))
}

#' Spectral radius of the linearized closed loop
#'
#' @inheritParams closed_loop_matrix
#' @return The largest eigenvalue modulus; the loop is stable below 1.
#' @export
closed_loop_radius <- function(lin, controller = c("h3a", "h3b"), scale, gain,
                               corrected_index = 1L) {
  m <- closed_loop_matrix(lin, controller, scale, gain, corrected_index)
  max(Mod(eigen(m, only.values = TRUE)$values))
}

#' Largest stable gain of the linearized closed loop on a grid
#'
#' @inheritParams closed_loop_matrix
#' @param grid gains scanned, in increasing order; the scan starts at the first positive
#'   gain.
#' @return The last gain of the grid before the first unstable one (spectral radius of at
#'   least 1), or `NA` when the first positive gain is unstable.
#' @export
stable_gain_limit <- function(lin, controller = c("h3a", "h3b"), scale,
                              grid = seq(0, 2, by = 0.01), corrected_index = 1L) {
  g <- grid[grid > 0]
  r <- vapply(g, function(x) closed_loop_radius(lin, controller, scale, x, corrected_index),
              numeric(1))
  bad <- which(r >= 1)
  if (length(bad) == 0L) return(max(g))
  if (bad[1] == 1L) return(NA_real_)
  g[bad[1] - 1L]
}
