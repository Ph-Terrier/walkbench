# Title: Adapter of model M1, the simplest walker (Garcia et al. 1998; Su and Dingwell 2007)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: exposes M1 through the common interface (specification, section 3). The
#   functions of the feasibility study (frozen functions) are called from the pinned,
#   unmodified copy of feasibility/scripts/00_functions_walker.R, sourced into a private
#   environment at first use. The cycle loop m1_simulate_loop() repeats the loop of the
#   frozen simulate_walker(), call for call, and adds the closed-loop port and the
#   metronome; with a fixed input and no controller it gives the same values.
# Dependencies: deSolve (attached by the pinned walker file), RANN, MASS
# Input: inst/pinned/simaci/feasibility/scripts/00_functions_walker.R and its racing/
#   subfolder
# Output: none

walker_pinned_path <- "simaci/feasibility/scripts/00_functions_walker.R"

# Environment holding the frozen functions of M1 (pinned copy, checked, cached)
m1_walker_env <- function() {
  if (!is.null(.wb$walker)) return(.wb$walker)
  path <- pinned_file(walker_pinned_path)
  pinned_file(racing_pinned_path)  # sourced by the walker file from its racing/ subfolder
  env <- new.env(parent = globalenv())
  source(path, local = env)
  .wb$walker <- env
  env
}

# Primary estimation settings of M1 (analysis plan, table 5.1; Su and Dingwell 2007)
settings_m1 <- function() {
  list(state = c("thG", "thGd", "phG", "phGd"), embedding = NULL,
       resample = list(method = "none"), window_unit = "step", theiler = 2,
       horizon = 4, window_s = c(0, 0.1), window_l = c(3, 4), slope = "mean_local",
       smoothing = "hamming7", pairs = "shrinking")
}

# Run object of the package -> list in the format of the frozen simulate_walker()
as_m1_sim <- function(run) {
  smp <- run$samples
  colnames(smp)[colnames(smp) == "cycle"] <- "step"
  ev <- data.frame(step = run$events$cycle, t_start = run$events$t_start,
                   t_end = run$events$t_end, duration = run$events$duration,
                   eps = run$events$u, stance = run$events$leg)
  list(samples = smp, events = ev, bounds = run$bounds, fell = run$fell,
       n_steps_done = run$n_cycles_done, dt = run$dt, rho = NA_real_)
}

# Cycle loop of M1: same calls as the frozen simulate_walker(), plus the two ports
m1_simulate_loop <- function(w, gamma, dt, t_min, variant, n_cycles, y0,
                             u_values = NULL, controller = NULL, metronome = NULL) {
  t_glob <- 0
  y_cur <- y0
  stance <- 1L
  samp_list <- vector("list", n_cycles)
  bounds <- vector("list", n_cycles)
  ev_t_start <- ev_t_end <- ev_dur <- ev_u <- rep(NA_real_, n_cycles)
  ev_exo <- ev_corr <- ev_dev <- ev_asy <- ev_beat <- rep(NA_real_, n_cycles)
  ev_leg <- rep(NA_integer_, n_cycles)
  x_post <- matrix(NA_real_, n_cycles, 2L)
  fell <- FALSE
  fall_cycle <- NA_integer_
  n_done <- 0L
  y_pre_prev <- NULL
  dur_prev <- NA_real_
  a_prev <- if (is.null(metronome)) NA_real_ else t_glob - metronome$beat(0L)
  for (k in seq_len(n_cycles)) {
    # input of cycle k: fixed series, or controller called at contact k - 1
    if (is.null(controller)) {
      u_k <- u_values[k]
    } else {
      out <- controller(k, list(k = k, t_contact = t_glob, x_post = y_cur[1:2],
                                y_post = y_cur, y_pre = y_pre_prev, asynchrony = a_prev,
                                duration_prev = dur_prev))
      u_k <- out$u
      if (length(u_k) != 1L || !is.finite(u_k)) {
        stop(sprintf("controller returned a non-finite input at cycle %d", k))
      }
      ev_exo[k] <- out$u_exo %||% NA_real_
      ev_corr[k] <- out$u_corr %||% NA_real_
      ev_dev[k] <- out$deviation %||% NA_real_
    }
    if (variant == "terrain") {
      st <- w$integrate_step_terrain(y_cur, gamma, u_k, dt = dt, t_offset = t_glob,
                                     t_min = t_min)
    } else if (variant == "slope") {
      st <- w$integrate_step(y_cur, gamma - u_k / 2, eps = 0, dt = dt, t_offset = t_glob,
                             t_min = t_min)
    } else {
      st <- w$integrate_step(y_cur, gamma, u_k, dt = dt, t_offset = t_glob, t_min = t_min)
    }
    y_start_cont <- w$to_continuous(matrix(y_cur, 1), stance)
    if (!is.null(st$samples)) {
      sm <- st$samples
      cont <- w$to_continuous(sm[, 2:5, drop = FALSE], stance)
      samp_list[[k]] <- cbind(t = sm[, 1] + t_glob, cont, cycle = k)
    }
    if (st$fell) {
      fell <- TRUE
      fall_cycle <- k
      break
    }
    y_end_cont <- w$to_continuous(matrix(st$y_end, 1), stance)
    ev_t_start[k] <- t_glob
    ev_t_end[k] <- t_glob + st$t_end
    ev_dur[k] <- st$t_end
    ev_u[k] <- u_k
    ev_leg[k] <- stance
    x_post[k, ] <- y_cur[1:2]
    bounds[[k]] <- list(start = y_start_cont, end = y_end_cont)
    map_variant <- if (variant %in% c("terrain", "slope")) "geometric" else variant
    y_pre_prev <- st$y_end
    y_cur <- w$heelstrike_map(st$y_end, variant = map_variant, eps = u_k)
    t_glob <- t_glob + st$t_end
    dur_prev <- st$t_end
    if (!is.null(metronome)) {
      ev_beat[k] <- metronome$beat(k)
      ev_asy[k] <- t_glob - ev_beat[k]
      a_prev <- ev_asy[k]
    }
    stance <- 3L - stance
    n_done <- k
  }
  idx <- seq_len(n_done)
  samples <- do.call(rbind, samp_list[!vapply(samp_list, is.null, TRUE)])
  events <- data.frame(cycle = idx, t_start = ev_t_start[idx], t_end = ev_t_end[idx],
                       duration = ev_dur[idx], u = ev_u[idx], u_exo = ev_exo[idx],
                       u_corr = ev_corr[idx], deviation = ev_dev[idx],
                       asynchrony = ev_asy[idx], beat = ev_beat[idx], leg = ev_leg[idx],
                       theta_post = x_post[idx, 1], theta_dot_post = x_post[idx, 2])
  list(samples = samples, events = events, bounds = bounds[idx], fell = fell,
       fall_cycle = fall_cycle, n_cycles_done = n_done)
}

#' Model M1, the simplest walker with foot-strike perturbations
#'
#' Adapter of the simplest walking model (Garcia et al. 1998) with the perturbed
#' foot-strike condition of Su and Dingwell (2007), as implemented in the SIM-ACI
#' feasibility study. Units: dimensionless time (unit sqrt(L/g)), lengths in leg lengths,
#' angles in radians. The cycle of the perturbation port is the step.
#'
#' @param gamma slope of the ramp (rad).
#' @param dt sampling step of the trajectory (model time).
#' @param t_min earliest time of a heel strike within a step (model time).
#' @return A list of class `simaci_model` (specification, section 3).
#' @export
model_m1 <- function(gamma = 0.009, dt = 0.03, t_min = 1.0) {
  w <- m1_walker_env()
  cache <- new.env(parent = emptyenv())
  injection_points <- list(
    foot_strike_timing = list(
      variant = "timing", value_unit = "rad", default = TRUE, linearizable = TRUE,
      description = "epsilon in the foot-strike condition phi - 2 theta = epsilon, literal Eq. 3 of Su and Dingwell (2007)"),
    foot_strike_geometric = list(
      variant = "geometric", value_unit = "rad", default = FALSE, linearizable = FALSE,
      description = "same condition, collision evaluated at the actual contact configuration"),
    foot_strike_full = list(
      variant = "full", value_unit = "rad", default = FALSE, linearizable = TRUE,
      description = "geometric collision plus rotation to the local slope"),
    slope = list(
      variant = "slope", value_unit = "rad", default = FALSE, linearizable = TRUE,
      description = "slope of the step set to gamma - epsilon / 2"),
    terrain = list(
      variant = "terrain", value_unit = "leg length", default = FALSE, linearizable = FALSE,
      description = "difference of bump heights between two contacts"))
  variant_of <- function(injection) {
    ip <- injection_points[[injection]]
    if (is.null(ip)) stop("unknown injection point of M1: ", injection)
    ip$variant
  }
  post_contact_state <- function(x) {
    c(x[1], x[2], 2 * x[1], x[2] * (1 - cos(2 * x[1])))
  }
  fixed_point <- function() {
    if (is.null(cache$fp)) {
      fp <- w$find_fixed_point(gamma)
      cache$fp <- list(x_star = c(fp$theta, fp$theta_dot), period = fp$period,
                       y0 = fp$y0, eig = fp$eig_step_map)
    }
    cache$fp
  }
  cycle_map <- function(x, u, injection = "foot_strike_timing") {
    v <- variant_of(injection)
    if (!injection_points[[injection]]$linearizable) {
      stop("no reduced cycle map for the injection point ", injection)
    }
    if (v == "slope") {
      st <- w$integrate_step(post_contact_state(x), gamma - u / 2, eps = 0, dt = 1,
                             t_min = t_min)
      y_new <- w$heelstrike_map(st$y_end, variant = "geometric", eps = u)
    } else {
      st <- w$integrate_step(post_contact_state(x), gamma, eps = u, dt = 1, t_min = t_min)
      y_new <- w$heelstrike_map(st$y_end, variant = v, eps = u)
    }
    if (st$fell) stop("fall during cycle_map()")
    list(x_next = y_new[1:2], duration = st$t_end)
  }
  flow <- function(x, u, ell, injection = "foot_strike_timing") {
    v <- variant_of(injection)
    g_eff <- if (v == "slope") gamma - u / 2 else gamma
    out <- deSolve::lsoda(post_contact_state(x), ell, w$walker_rhs, list(gamma = g_eff),
                          rtol = 1e-11, atol = 1e-13)
    unname(out[, 2:5])
  }
  rhs <- function(y) w$walker_rhs(0, y, list(gamma = gamma))[[1]]
  simulate <- function(input = NULL, n_cycles, y0, controller = NULL, metronome = NULL,
                       injection = "foot_strike_timing") {
    v <- variant_of(injection)
    u_values <- if (is.null(input)) NULL else input$values[seq_len(n_cycles)]
    m1_simulate_loop(w, gamma, dt, t_min, v, n_cycles, y0, u_values = u_values,
                     controller = controller, metronome = metronome)
  }
  normalize_cycles <- function(run) w$normalize_strides(as_m1_sim(run))
  structure(list(
    id = "M1", label = "simplest walker (Garcia et al. 1998; Su and Dingwell 2007)",
    version = "0.1.0", params = list(gamma = gamma, dt = dt, t_min = t_min),
    time_unit = "dimensionless time, unit sqrt(L/g)", cycle_unit = "step",
    cycles_per_stride = 2L, state_names = c("thG", "thGd", "phG", "phGd"),
    reduced_state_names = c("theta", "theta_dot"), injection_points = injection_points,
    default_injection = "foot_strike_timing", fixed_point = fixed_point,
    post_contact_state = post_contact_state, cycle_map = cycle_map, flow = flow,
    rhs = rhs, coord_maps = list(t(unname(w$to_continuous(diag(4), 1L))),
                                 t(unname(w$to_continuous(diag(4), 2L)))),
    simulate = simulate, normalize_cycles = normalize_cycles, corrected_index = 1L,
    estimation_settings = settings_m1(),
    fall_rule = "abs(theta) >= 1.2 rad, or no heel strike within 15 time units"),
    class = "simaci_model")
}

#' Models available in the package
#'
#' @return A named list of model adapters (class `simaci_model`).
#' @export
list_models <- function() {
  list(M1 = model_m1(), M2 = model_m2())
}
