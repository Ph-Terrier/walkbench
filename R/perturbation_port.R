# Title: Perturbation port: input objects, the two generators of the feasibility study, checks
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: an input is one value per cycle of the model, applied without rescaling at
#   the injection point named by the run (specification, section 4). The generators of AR(1)
#   series, real stride series and surrogates belong to SP2.1 and SP2.6 and return the same
#   object.
#   walkbench 0.4.0 (2026-11-06): rules of the analysis plan frozen at commit ba72afe
#   (osf.io/m74tk). input_arfima(n_transient = ) centers and rescales a realization on its part
#   after the transient (section 4.3). input_fixed_series() rescales a fixed series and fills
#   the transient with the last values of the series, the circular start of section 4.8, which
#   replaces choice O2 of the interface specification. input_surrogate() draws a random
#   permutation with the seed rule of the surrogates (section 4.8).
# Dependencies: stats; the frozen arfima_series() of the pinned walker file
# Input: none
# Output: none

#' Input series of the perturbation port
#'
#' @param values numeric vector, one value per cycle.
#' @param source label of the source, for example `"arfima"` or `"real_series"`.
#' @param params list of the parameters of the generator.
#' @param seed seed of the generator, or `NA` for a fixed series.
#' @param redrawable `TRUE` when a fall may be followed by a new draw.
#' @param value_unit unit of the values.
#' @param cycle_unit cycle of the series, `"step"` or `"stride"`; it must equal that of
#'   the model.
#' @return A list of class `simaci_input`.
#' @export
simaci_input <- function(values, source, params = list(), seed = NA_integer_,
                         redrawable = TRUE, value_unit = NA_character_,
                         cycle_unit = "step") {
  if (!is.numeric(values)) stop("values must be numeric")
  structure(list(values = as.numeric(values), source = source, params = params,
                 seed = seed, redrawable = redrawable, value_unit = value_unit,
                 cycle_unit = cycle_unit), class = "simaci_input")
}

#' Uniform i.i.d. input, as drawn in the amplitude experiment of the feasibility study
#'
#' @param n number of values.
#' @param rho amplitude: the values are `runif(n, -0.5, 0.5) * rho`.
#' @param seed seed passed to `set.seed()`; `NULL` continues the current stream.
#' @param cycle_unit cycle of the series.
#' @return A `simaci_input`.
#' @export
input_iid_uniform <- function(n, rho, seed = NULL, cycle_unit = "step") {
  if (!is.null(seed)) set.seed(seed)
  simaci_input(stats::runif(n, -0.5, 0.5) * rho, source = "iid_uniform",
               params = list(n = n, rho = rho), seed = seed %||% NA_integer_,
               redrawable = TRUE, value_unit = "rad", cycle_unit = cycle_unit)
}

#' Exact Gaussian ARFIMA(0,d,0) input
#'
#' Calls the frozen `arfima_series()` of the feasibility study: Hosking autocovariance,
#' Cholesky factor, one realization of `n` values. With `n_transient = 0` (default), the
#' realization is centered and rescaled as a whole to the standard deviation `sd`, as in the
#' feasibility study and in the calibration runs of M2. With `n_transient > 0`, it is centered
#' and rescaled so that its values after the transient have the mean 0 and the standard
#' deviation `sd` (analysis plan frozen at commit ba72afe, section 4.3); the transient values
#' undergo the same affine map. The draws of the random stream are the same in both cases.
#'
#' @param n number of values, transient included.
#' @param d memory parameter.
#' @param sd standard deviation of the series, or of its part after the transient.
#' @param seed seed passed to `set.seed()`; `NULL` continues the current stream.
#' @param cycle_unit cycle of the series.
#' @param n_transient number of transient values at the start of the series.
#' @return A `simaci_input`.
#' @export
input_arfima <- function(n, d, sd, seed = NULL, cycle_unit = "step", n_transient = 0L) {
  if (n_transient < 0 || n_transient > n - 2L) stop("n_transient must lie in 0 to n - 2")
  w <- m1_walker_env()
  if (n_transient == 0) {
    x <- w$arfima_series(n, d, sd_target = sd, seed = seed)
  } else {
    x <- w$arfima_series(n, d, sd_target = 1, seed = seed)
    kept <- x[(n_transient + 1L):n]
    x <- (x - mean(kept)) / stats::sd(kept) * sd
  }
  simaci_input(x, source = "arfima",
               params = list(n = n, d = d, sd = sd, n_transient = n_transient,
                             rescaled_on = if (n_transient == 0) "whole series" else
                               "values after the transient"),
               seed = seed %||% NA_integer_, redrawable = TRUE, value_unit = "rad",
               cycle_unit = cycle_unit)
}

#' Fixed series with a circular start
#'
#' Rule of the analysis plan frozen at commit ba72afe (section 4.8) for the real stride
#' series: the window of the series (for example its first 1,200 strides) is centered,
#' divided by its standard deviation and multiplied by `sd`. The last `n_transient` values of
#' the window then precede it and fill the transient. The input cannot be redrawn after a fall.
#'
#' @param values the window of the series, one value per cycle of the model.
#' @param n_transient number of transient cycles.
#' @param sd standard deviation of the window after rescaling.
#' @param source label of the source.
#' @param params list of parameters recorded with the input.
#' @param value_unit unit of the values after rescaling.
#' @param cycle_unit cycle of the series.
#' @return A `simaci_input` of `length(values) + n_transient` values.
#' @export
input_fixed_series <- function(values, n_transient, sd, source = "real_series",
                               params = list(), value_unit = NA_character_,
                               cycle_unit = "step") {
  values <- as.numeric(values)
  n <- length(values)
  if (n < 3L || any(!is.finite(values))) stop("values must be at least 3 finite numbers")
  if (n_transient < 0 || n_transient > n) stop("n_transient must lie in 0 to length(values)")
  z <- (values - mean(values)) / stats::sd(values) * sd
  x <- c(utils::tail(z, n_transient), z)
  simaci_input(x, source = source,
               params = c(params, list(n_window = n, n_transient = n_transient, sd = sd,
                                       transient = "circular start: last values of the window")),
               seed = NA_integer_, redrawable = FALSE, value_unit = value_unit,
               cycle_unit = cycle_unit)
}

#' Seed of a surrogate
#'
#' `70,000,000 + 1,000 * s + i` for permutation `i` of series `s` (analysis plan frozen at
#' commit ba72afe, section 4.8); permutations 1 to 10 are the surrogates, and a surrogate
#' that falls is replaced by the permutations from `i = 11` upward.
#'
#' @param s index of the series, from 1.
#' @param i index of the permutation, from 1 to 999.
#' @return The seed, an integer.
#' @export
seed_surrogate <- function(s, i) {
  if (any(s < 1 | s > 29999)) stop("s must lie in 1 to 29,999")
  if (any(i < 1 | i > 999)) stop("i must lie in 1 to 999")
  as.integer(70000000 + 1000 * s + i)
}

#' Surrogate of a fixed series: random permutation of its window
#'
#' After `set.seed(seed_surrogate(s, i))`, one call of `sample()` permutes the values of the
#' window. The permutation keeps the values and removes the temporal order. The result is
#' then rescaled and given a circular start by [input_fixed_series()].
#'
#' @inheritParams input_fixed_series
#' @inheritParams seed_surrogate
#' @return A `simaci_input`.
#' @export
input_surrogate <- function(values, s, i, n_transient, sd, value_unit = NA_character_,
                            cycle_unit = "step") {
  seed <- seed_surrogate(s, i)
  set.seed(seed)
  perm <- sample(as.numeric(values))
  inp <- input_fixed_series(perm, n_transient, sd, source = "surrogate",
                            params = list(series = s, permutation = i,
                                          sample_kind = RNGkind()[3]),
                            value_unit = value_unit, cycle_unit = cycle_unit)
  inp$seed <- seed
  inp
}

#' Check an input against a model and a number of cycles
#'
#' @param input a `simaci_input`.
#' @param model a `simaci_model`.
#' @param n_cycles number of cycles to simulate.
#' @return `TRUE`, invisibly; the function stops on a failed check.
#' @export
check_input <- function(input, model, n_cycles) {
  if (!inherits(input, "simaci_input")) stop("the input must be a simaci_input")
  if (length(input$values) < n_cycles) {
    stop(sprintf("input too short: %d values for %d cycles", length(input$values), n_cycles))
  }
  if (!all(is.finite(input$values[seq_len(n_cycles)]))) stop("the input has non-finite values")
  if (!identical(input$cycle_unit, model$cycle_unit)) {
    stop(sprintf("cycle unit of the input (%s) differs from that of the model (%s)",
                 input$cycle_unit, model$cycle_unit))
  }
  invisible(TRUE)
}
