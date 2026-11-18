# Title: Adapter of model M2, the walker with arced feet and hip spring (Bruijn et al. 2012)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), which carries out
#   SP1.3 on the author's decision of 2026-11-05, without a code-generation model
# Date: 2026-11-06
# Description: extended simplest walker of Hobbelen and Wisse (2007, appendix), used by Bruijn
#   et al. (2012): point hip mass M = 1, legs of length 1, arced feet of radius r_f centred on
#   the leg at 1 - r_f from the hip, stance foot rolling without slipping, foot masses m -> 0 at
#   the ends of the legs, hip torsional spring of stiffness k (normalized by the foot mass),
#   slope gamma through tilted gravity (M = l = g = 1). The equations were derived by the SP1.1
#   session (SIMACI/wp1_replication/04_m2_equations_check.py):
#   - theta_dd equals the printed equation of Hobbelen and Wisse (2007, p. 1223);
#   - phi_dd equals the printed equation with the sign of its third term reversed; the printed
#     sign does not reduce to the simplest walker at r_f = 0;
#   - the impact map conserves the angular momentum of the hip mass about the new contact
#     point and of the old stance foot mass about the hip; it equals the printed map on level
#     ground and differs from it by a term of order r_f h after a floor-height step h.
#   Option impact = "printed" (added by the SP1.1 session on 2026-11-06, on the author's
#   approval of a test of the printed map): the impact map as printed by Hobbelen and Wisse
#   (2007, p. 1223), in which every denominator is evaluated at theta-; it equals the derived
#   map on level ground and at r_f = 0. The derived map stays the default.
#   The perturbation is the height h of the floor at the new contact, relative to the stance
#   foot (Hobbelen and Wisse 2007: "a floor height difference h that influences the instant of
#   footstrike"), one value per step.
#   Primary estimation settings (walkbench 0.2.0, 2026-11-06): settings_m2() returns those of
#   table 5.1 of the analysis plan, draft 0.4, approved by the author on 2026-11-06 at 12:26
#   (register SIMACI/docs/author_decisions.md): the state variables of the model referred to
#   the stance leg, recovered by m2_model_coords(); settings_m2("continuous") returns the
#   settings of draft 0.3, used by the replication runs with walkbench 0.1.x.
# Dependencies: deSolve
# Input: none
# Output: none

# Continuous leg coordinates (Goswami convention), as the frozen to_continuous() of M1
m2_leg_coords <- function(mat, stance_leg) {
  th <- mat[, 1]; thd <- mat[, 2]; ph <- mat[, 3]; phd <- mat[, 4]
  if (stance_leg == 1) {
    cbind(thG = th, thGd = thd, phG = -ph, phGd = -phd)
  } else {
    cbind(thG = th - ph, thGd = thd - phd, phG = ph, phGd = phd)
  }
}

# State variables of the model (theta, theta_dot, phi, phi_dot), referred to the stance leg,
# from the continuous leg coordinates: inverse of m2_leg_coords() for the same stance leg
m2_model_coords <- function(mat, stance_leg) {
  g1 <- mat[, 1]; g2 <- mat[, 2]; g3 <- mat[, 3]; g4 <- mat[, 4]
  if (stance_leg == 1) {
    cbind(theta = g1, theta_dot = g2, phi = -g3, phi_dot = -g4)
  } else {
    cbind(theta = g1 + g3, theta_dot = g2 + g4, phi = g3, phi_dot = g4)
  }
}

# Maps of the estimation settings, from the sampled coordinates to the state variables of the
# model, one matrix per stance leg (row vectors: y = c %*% t(map))
m2_model_coordinate_maps <- function() {
  list(names = c("theta", "theta_dot", "phi", "phi_dot"),
       sampled = c("thG", "thGd", "phG", "phGd"),
       maps = list(t(unname(m2_model_coords(diag(4), 1L))),
                   t(unname(m2_model_coords(diag(4), 2L)))))
}

# Equations of motion between foot strikes; state y = (theta, theta_dot, phi, phi_dot)
m2_rhs <- function(t, y, parms) {
  th <- y[1]; thd <- y[2]; ph <- y[3]; phd <- y[4]
  r <- parms$r_f; gam <- parms$gamma; k <- parms$k
  dd <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(th)
  thdd <- ((1 - r) * sin(th - gam) + r * (1 - r) * sin(th) * thd^2 - r * sin(gam)) / dd
  psi <- th - ph  # angle of the swing leg with the slope normal
  phdd <- thdd + sin(psi - gam) - thdd * (r * cos(psi) + (1 - r) * cos(ph)) +
    (1 - r) * thd^2 * sin(ph) - k * ph
  list(c(thd, thdd, phd, phdd))
}

# Impact at foot strike, with exchange of the legs
m2_impact <- function(y, r) {
  thm <- y[1]; thdm <- y[2]; phm <- y[3]
  thp <- thm - phm
  dp <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(thp)
  lm <- thdm * ((1 - r)^2 * cos(phm) + r * (1 - r) * (cos(thm - phm) + cos(thm)) + r^2)
  thdp <- lm / dp
  phdp <- thdp * (1 - (1 - r) * cos(phm) - r * cos(thm)) - r * (1 - cos(thm)) * thdm
  c(thp, thdp, -phm, phdp)
}

# Impact map as printed by Hobbelen and Wisse (2007, appendix, p. 1223), transcribed from the
# PDF of the Zotero group SIM-ACI (attachment 6QP8K5SE) and checked against it on 2026-11-06;
# the denominator D(theta-) replaces D(theta+) of the derived map
m2_impact_printed <- function(y, r) {
  thm <- y[1]; thdm <- y[2]; phm <- y[3]
  dm <- (1 - r)^2 + r^2 + 2 * r * (1 - r) * cos(thm)
  thdp <- ((1 - r)^2 * cos(phm) + r * (1 - r) * (cos(thm - phm) + cos(thm)) + r^2) / dm * thdm
  phdp <- (1 - r) * ((-r * (1 - r) + (1 - r) * cos(phm) * (1 - (1 - r) * cos(phm))) / dm +
                       (r * (cos(thm) - cos(thm - phm)) + r * (1 - r) * sin(thm) * sin(phm)) / dm +
                       (r * (sin(thm) - sin(thm - phm)) * (r * sin(thm) + (1 - r) * sin(phm))) / dm) *
    thdm
  c(thm - phm, thdp, -phm, phdp)
}

# One step from the post-strike state y0, until the swing foot meets the floor at height h:
# (1 - r)(cos(theta) - cos(theta - phi)) = h, with the foot descending, the swing leg in front
# (theta < th_guard) and after the early swing (t > t_min); same structure as the frozen
# integrate_step_terrain() of M1.
m2_integrate_step <- function(y0, parms, h, dt = 0.03, t_offset = 0, t_min = 1.0,
                              th_guard = -0.05, t_max = 15, rtol = 1e-10, atol = 1e-12) {
  r <- parms$r_f
  rootfun <- function(t, y, parms) {
    c((1 - r) * (cos(y[1]) - cos(y[1] - y[3])) - h, abs(y[1]) - 1.2)
  }
  t_loc <- 0
  y_cur <- y0
  samples <- NULL
  repeat {
    first <- ceiling((t_offset + t_loc - 1e-12) / dt) * dt - t_offset
    grid <- if (first <= t_max) seq(first, t_max, by = dt) else numeric(0)
    grid <- grid[grid > t_loc + 1e-12]
    # no sampling point left before t_max: the step did not end in time (post-generation fix
    # by Claude Opus 5.5, 2026-11-06: lsodar stops with "illegal input" on a single time)
    if (length(grid) == 0L) {
      return(list(fell = TRUE, samples = samples, t_end = t_max, y_end = NULL))
    }
    out <- deSolve::lsodar(y_cur, c(t_loc, grid), m2_rhs, parms, rootfunc = rootfun,
                           rtol = rtol, atol = atol)
    troot <- attributes(out)$troot
    n_out <- nrow(out)
    on_grid <- abs((out[, 1] + t_offset) / dt - round((out[, 1] + t_offset) / dt)) < 1e-9
    keep <- which(on_grid & out[, 1] > t_loc + 1e-12)
    if (length(keep) > 0) samples <- rbind(samples, out[keep, , drop = FALSE])
    if (is.null(troot) || length(troot) == 0) {
      return(list(fell = TRUE, samples = samples, t_end = t_max, y_end = NULL))
    }
    y_root <- out[n_out, 2:5]
    if (abs(y_root[1]) >= 1.2 - 1e-6) {
      return(list(fell = TRUE, samples = samples, t_end = troot, y_end = NULL))
    }
    th <- y_root[1]; thd <- y_root[2]; ph <- y_root[3]; phd <- y_root[4]
    rate <- (1 - r) * (-sin(th) * thd + sin(th - ph) * (thd - phd))  # d(foot height)/dt
    if (troot > t_min && rate < 0 && th < th_guard) {
      return(list(fell = FALSE, samples = samples, t_end = troot, y_end = y_root))
    }
    nudge <- deSolve::lsodar(y_root, c(troot, troot + 1e-6), m2_rhs, parms, rtol = rtol,
                             atol = atol)
    y_cur <- nudge[nrow(nudge), 2:5]
    t_loc <- troot + 1e-6
    if (t_loc >= t_max) {
      return(list(fell = TRUE, samples = samples, t_end = t_max, y_end = NULL))
    }
  }
}

# Cycle loop of M2, with the same outputs and ports as the loop of M1
m2_simulate_loop <- function(parms, dt, t_min, n_cycles, y0, u_values = NULL,
                             controller = NULL, metronome = NULL, impact_fun = m2_impact) {
  t_glob <- 0
  y_cur <- y0
  stance <- 1L
  samp_list <- vector("list", n_cycles)
  bounds <- vector("list", n_cycles)
  ev_t_start <- ev_t_end <- ev_dur <- ev_u <- rep(NA_real_, n_cycles)
  ev_exo <- ev_corr <- ev_dev <- ev_asy <- ev_beat <- rep(NA_real_, n_cycles)
  ev_leg <- rep(NA_integer_, n_cycles)
  x_post <- matrix(NA_real_, n_cycles, 4L)
  fell <- FALSE
  fall_cycle <- NA_integer_
  n_done <- 0L
  y_pre_prev <- NULL
  dur_prev <- NA_real_
  a_prev <- if (is.null(metronome)) NA_real_ else t_glob - metronome$beat(0L)
  for (k in seq_len(n_cycles)) {
    if (is.null(controller)) {
      u_k <- u_values[k]
    } else {
      out <- controller(k, list(k = k, t_contact = t_glob, x_post = y_cur, y_post = y_cur,
                                y_pre = y_pre_prev, asynchrony = a_prev,
                                duration_prev = dur_prev))
      u_k <- out$u
      if (length(u_k) != 1L || !is.finite(u_k)) {
        stop(sprintf("controller returned a non-finite input at cycle %d", k))
      }
      ev_exo[k] <- out$u_exo %||% NA_real_
      ev_corr[k] <- out$u_corr %||% NA_real_
      ev_dev[k] <- out$deviation %||% NA_real_
    }
    st <- m2_integrate_step(y_cur, parms, u_k, dt = dt, t_offset = t_glob, t_min = t_min)
    y_start_cont <- m2_leg_coords(matrix(y_cur, 1), stance)
    if (!is.null(st$samples)) {
      sm <- st$samples
      samp_list[[k]] <- cbind(t = sm[, 1] + t_glob,
                              m2_leg_coords(sm[, 2:5, drop = FALSE], stance), cycle = k)
    }
    if (st$fell) {
      fell <- TRUE
      fall_cycle <- k
      break
    }
    ev_t_start[k] <- t_glob
    ev_t_end[k] <- t_glob + st$t_end
    ev_dur[k] <- st$t_end
    ev_u[k] <- u_k
    ev_leg[k] <- stance
    x_post[k, ] <- y_cur
    bounds[[k]] <- list(start = y_start_cont,
                        end = m2_leg_coords(matrix(st$y_end, 1), stance))
    y_pre_prev <- st$y_end
    y_cur <- impact_fun(st$y_end, parms$r_f)
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
                       theta_post = x_post[idx, 1], theta_dot_post = x_post[idx, 2],
                       phi_post = x_post[idx, 3], phi_dot_post = x_post[idx, 4])
  list(samples = samples, events = events, bounds = bounds[idx], fell = fell,
       fall_cycle = fall_cycle, n_cycles_done = n_done)
}

#' Estimation settings of model M2
#'
#' `space = "model"` gives the primary settings of M2, table 5.1 of the SIM-ACI analysis plan
#' (draft 0.4, approved by the author on 2026-11-06 at 12:26). Bruijn et al. (2012, p. 430)
#' state the state space, the resampling to 100 samples per stride and the windows; the other
#' settings follow `lds_calc.m`, the public code of S. M. Bruijn
#' (github.com/SjoerdBruijn/LocalDynamicStability, read on 2026-11-06):
#' - state space: the four state variables of the model (theta, theta_dot, phi, phi_dot),
#'   referred to the stance leg, without embedding. They jump at each foot strike, where the
#'   legs exchange their roles;
#' - resampling: the uniform grid of 100 samples per stride on average over the whole run;
#'   each grid point is interpolated inside its own step, so that the jumps stay jumps;
#' - neighbors: one nearest neighbor outside half a stride on each side (50 samples);
#' - curve: mean of the ln distances over a shrinking set of pairs (RACING engine);
#' - slopes: least squares over 0 to 0.5 stride (lambda_S) and 4 to 10 strides (lambda_L).
#'
#' `space = "continuous"` gives the settings of draft 0.3 of the plan, which the replication
#' runs of M2 used with walkbench 0.1.x: the continuous leg coordinates, a global resampling
#' across the foot strikes and a neighbor exclusion of one stride. The plan keeps them as a
#' sensitivity analysis (section 5.1).
#'
#' @param space `"model"` (primary settings) or `"continuous"`.
#' @return A list of estimation settings for [estimate_divergence()].
#' @export
settings_m2 <- function(space = c("model", "continuous")) {
  space <- match.arg(space)
  if (space == "continuous") {
    return(list(state = c("thG", "thGd", "phG", "phGd"), embedding = NULL,
                resample = list(method = "global", n_per = 100, per = "stride"),
                window_unit = "stride", theiler = 1, horizon = 10, window_s = c(0, 0.5),
                window_l = c(4, 10), slope = "lsq", smoothing = "none", pairs = "shrinking"))
  }
  list(state = c("theta", "theta_dot", "phi", "phi_dot"), coordinates = "model",
       model_coordinates = m2_model_coordinate_maps(), embedding = NULL,
       resample = list(method = "global_by_cycle", n_per = 100, per = "stride"),
       window_unit = "stride", theiler = 0.5, horizon = 10, window_s = c(0, 0.5),
       window_l = c(4, 10), slope = "lsq", smoothing = "none", pairs = "shrinking")
}

#' Model M2, the walker with arced feet and hip spring
#'
#' Adapter of the extended simplest walking model of Hobbelen and Wisse (2007), used by Bruijn
#' et al. (2012). Units: dimensionless time (unit sqrt(L/g)), lengths in leg lengths, angles
#' in radians. The cycle of the perturbation port is the step; the input is the height of the
#' floor at the next contact, relative to the stance foot, in leg lengths.
#'
#' @param gamma slope of the ramp (rad).
#' @param k stiffness of the hip spring, normalized by the foot mass.
#' @param r_f radius of the arced feet (leg lengths), below 1.
#' @param dt sampling step of the trajectory (model time).
#' @param t_min earliest time of a heel strike within a step (model time). The value 0.25 lies
#'   below the shortest step of the stiffness manipulation (about 1 time unit at k = 10); the
#'   strike also requires the swing leg in front of the stance leg (theta < -0.05), which
#'   excludes the scuffing of the swing foot at mid-stance.
#' @param x_init initial guess of the post-strike state (theta, theta_dot, phi, phi_dot) for
#'   the search of the fixed point; by default, the expansion of Garcia et al. (1998).
#' @param relax_steps number of iterations of the step map before the Newton search.
#' @param impact impact map at foot strike: "derived" (default; conservation of angular
#'   momentum, derived by the SP1.1 session) or "printed" (the map printed by Hobbelen and
#'   Wisse 2007, p. 1223, whose denominators are evaluated at theta-). The two maps are equal
#'   on level ground and at r_f = 0.
#' @return A list of class `simaci_model` (specification, section 3).
#' @export
model_m2 <- function(gamma = 0.004, k = 0, r_f = 0, dt = 0.03, t_min = 0.25, x_init = NULL,
                     relax_steps = 200L, impact = c("derived", "printed")) {
  if (r_f < 0 || r_f >= 1) stop("r_f must lie in [0, 1)")
  impact <- match.arg(impact)
  impact_fun <- if (impact == "derived") m2_impact else m2_impact_printed
  parms <- list(gamma = gamma, k = k, r_f = r_f)
  cache <- new.env(parent = emptyenv())
  injection_points <- list(
    floor_height = list(variant = "floor_height", value_unit = "leg length", default = TRUE,
                        linearizable = TRUE,
                        description = "height of the floor at the next contact relative to the stance foot"))
  cycle_map <- function(x, u, injection = "floor_height") {
    st <- m2_integrate_step(x, parms, u, dt = 1, t_min = t_min)
    if (st$fell) stop("fall during cycle_map()")
    list(x_next = impact_fun(st$y_end, r_f), duration = st$t_end)
  }
  fixed_point <- function() {
    if (!is.null(cache$fp)) return(cache$fp)
    x <- x_init
    if (is.null(x)) {
      th0 <- 0.970956 * gamma^(1 / 3) - 0.270837 * gamma
      thd0 <- -1.045203 * 0.970956 * gamma^(1 / 3) + (1.045203 * 0.270837 + 1.062895) * gamma
      x <- c(th0, thd0, 2 * th0, thd0 * (1 - cos(2 * th0)))
    }
    safe_map <- function(z) tryCatch(cycle_map(z, 0)$x_next, error = function(e) NULL)
    # damped Newton iteration on x - S(x); returns NULL when it fails
    newton <- function(x) {
      for (i in seq_len(60)) {
        s0 <- safe_map(x)
        if (is.null(s0)) return(NULL)
        f0 <- s0 - x
        if (sqrt(sum(f0^2)) < 1e-12) return(x)
        jac <- matrix(0, 4, 4)
        for (j in 1:4) {
          xp <- x
          xp[j] <- xp[j] + 1e-7
          sp <- safe_map(xp)
          if (is.null(sp)) return(NULL)
          jac[, j] <- (sp - xp - f0) / 1e-7
        }
        step <- tryCatch(solve(jac, f0), error = function(e) NULL)
        if (is.null(step)) return(NULL)
        lambda <- 1
        repeat {
          xt <- x - lambda * step
          st <- safe_map(xt)
          if (!is.null(st) && sqrt(sum((st - xt)^2)) < sqrt(sum(f0^2))) break
          lambda <- lambda / 2
          if (lambda < 1e-4) return(NULL)
        }
        x <- xt
      }
      s0 <- safe_map(x)
      if (!is.null(s0) && sqrt(sum((s0 - x)^2)) < 1e-9) x else NULL
    }
    # relaxation: iterate the step map, which converges when the cycle is stable
    relax <- function(x) {
      for (i in seq_len(relax_steps)) {
        xn <- safe_map(x)
        if (is.null(xn)) return(NULL)
        if (sqrt(sum((xn - x)^2)) < 1e-9) return(xn)
        x <- xn
      }
      x
    }
    # relaxation first, which reaches the stable cycle when the guess lies in its basin; Newton
    # from the guess otherwise (for example after a change of parameters that makes the
    # walker fall from the previous cycle)
    s_guess <- safe_map(x)
    close <- !is.null(s_guess) && sqrt(sum((s_guess - x)^2)) < 1e-4
    xs <- if (close) newton(x) else NULL
    if (is.null(xs)) {
      xr <- relax(x)
      xs <- if (is.null(xr)) NULL else newton(xr)
    }
    if (is.null(xs) && !close) xs <- newton(x)
    if (is.null(xs)) stop("fixed point of M2 not found")
    x <- xs
    cm <- cycle_map(x, 0)
    if (sqrt(sum((cm$x_next - x)^2)) > 1e-9) stop("fixed point of M2 not found")
    s_map <- matrix(0, 4, 4)
    for (j in 1:4) {
      xp <- x
      xp[j] <- xp[j] + 1e-7
      s_map[, j] <- (cycle_map(xp, 0)$x_next - cm$x_next) / 1e-7
    }
    # theta just before the strike: the next post-strike angle (x[1]) plus phi- = -phi+ (-x[3])
    th_end <- x[1] - x[3]
    # hip displacement along the slope over one step: x_hip = x0 - r_f theta - (1 - r_f) sin(theta)
    cache$fp <- list(x_star = x, period = cm$duration, y0 = x, eig = eigen(s_map)$values,
                     step_length = -r_f * (th_end - x[1]) - (1 - r_f) * (sin(th_end) - sin(x[1])))
    cache$fp
  }
  flow <- function(x, u, ell, injection = "floor_height") {
    out <- deSolve::lsoda(x, ell, m2_rhs, parms, rtol = 1e-11, atol = 1e-13)
    unname(out[, 2:5])
  }
  rhs <- function(y) m2_rhs(0, y, parms)[[1]]
  simulate <- function(input = NULL, n_cycles, y0, controller = NULL, metronome = NULL,
                       injection = "floor_height") {
    u_values <- if (is.null(input)) NULL else input$values[seq_len(n_cycles)]
    m2_simulate_loop(parms, dt, t_min, n_cycles, y0, u_values = u_values,
                     controller = controller, metronome = metronome, impact_fun = impact_fun)
  }
  normalize_cycles <- function(run) m1_walker_env()$normalize_strides(as_m1_sim(run))
  structure(list(
    id = "M2", label = "walker with arced feet and hip spring (Hobbelen and Wisse 2007; Bruijn et al. 2012)",
    version = "0.2.0", params = list(gamma = gamma, k = k, r_f = r_f, dt = dt, t_min = t_min,
                                     impact = impact),
    time_unit = "dimensionless time, unit sqrt(L/g)", cycle_unit = "step",
    cycles_per_stride = 2L, state_names = c("thG", "thGd", "phG", "phGd"),
    reduced_state_names = c("theta", "theta_dot", "phi", "phi_dot"),
    injection_points = injection_points, default_injection = "floor_height",
    fixed_point = fixed_point, post_contact_state = function(x) x, cycle_map = cycle_map,
    flow = flow, rhs = rhs,
    coord_maps = list(t(unname(m2_leg_coords(diag(4), 1L))),
                      t(unname(m2_leg_coords(diag(4), 2L)))),
    simulate = simulate, normalize_cycles = normalize_cycles, corrected_index = 1L,
    estimation_settings = settings_m2(),
    fall_rule = "abs(theta) >= 1.2 rad, or no foot strike within 15 time units"),
    class = "simaci_model")
}
