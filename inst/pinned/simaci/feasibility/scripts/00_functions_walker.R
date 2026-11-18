# =============================================================================
# Title       : Core functions - simplest passive dynamic walker (Garcia 1998)
#               with stochastic foot-strike perturbations (Su & Dingwell 2007)
# Author      : Philippe Terrier (with Claude)
# Date        : 2026-08-20
# Description : Implements the 2D "simplest walking model" (Garcia et al. 1998,
#               J Biomech Eng 120:281-288) modified per Su & Dingwell (2007,
#               J Biomech Eng 129:802-810): random perturbations epsilon applied
#               to the foot-strike condition (phi - 2*theta = epsilon) simulate
#               walking down a bumpy slope. Provides simulation and the three
#               analyses of Su & Dingwell 2007: stride-to-stride variability
#               (MeanSD), orbital stability (max Floquet multipliers), and local
#               divergence exponents (Rosenstein algorithm, lambda_S and
#               lambda_L; divergence curve computed by the RACING engine,
#               scripts/racing/, used unmodified).
# Dependencies: deSolve, MASS, RANN; sources racing/rosenstein_divergence.R
# Input       : racing/rosenstein_divergence.R, MD5-checked
#               at source time (stops if RANN is missing);
#               functions only otherwise; sourced by 01_,
#               02_, 04_, doc/assets/simaci_results.R and
#               verif/02, 04, 07
# Output      : none
# Conventions : dimensionless units (lengths / leg length L, time / sqrt(L/g));
#               theta = stance leg angle w.r.t. slope normal, phi = swing leg
#               angle w.r.t. stance leg (Garcia 1998 convention).
# =============================================================================

library(deSolve)

# --- Equations of motion (Garcia 1998, Eq. 1a/1b in Su & Dingwell 2007) ------
# State y = (theta, theta_dot, phi, phi_dot); parms = list(gamma = slope angle)
walker_rhs <- function(t, y, parms) {
  th  <- y[1]; thd <- y[2]; ph <- y[3]; phd <- y[4]
  gamma <- parms$gamma
  thdd <- sin(th - gamma)
  phdd <- thdd + thd^2 * sin(ph) - cos(th - gamma) * sin(ph)
  list(c(thd, thdd, phd, phdd))
}

# --- Heel-strike transition map (Su & Dingwell 2007, Eq. 3) ------------------
# Legs are swapped; angular momentum conserved through plastic collision.
# Three readings of the perturbed foot strike (phi - 2*theta = eps):
#   "timing"    : literal Eq. 3 (coefficients in theta only); eps only shifts
#                 WHEN the strike occurs, angles reset to phi = 2*theta.
#   "geometric" : collision evaluated at the actual contact configuration
#                 (inter-leg angle phi- = 2*theta- + eps): new stance angle
#                 theta+ = theta- - phi-, impulse factor cos(phi-).
#   "full"      : complete bumpy-ground geometry = "geometric" collision PLUS
#                 rotation of the reference frame to the local effective slope
#                 of the next step (delta_gamma = -eps/2, Su & Dingwell 2007):
#                 theta+ = -theta- - eps/2, and phi+ = 2*theta+ holds exactly.
#                 The energetic effects of contact geometry and effective
#                 slope partially cancel, as on a real bumpy ramp.
# All three reduce to Eq. 3 exactly when eps = 0.
heelstrike_map <- function(y, variant = "full", eps = 0) {
  th <- y[1]; thd <- y[2]; ph <- y[3]
  if (variant == "timing") {
    c2t <- cos(2 * th)
    c(-th, thd * c2t, -2 * th, thd * c2t * (1 - c2t))
  } else if (variant == "geometric") {
    cp <- cos(ph)
    c(th - ph, thd * cp, -ph, thd * cp * (1 - cp))
  } else {
    cp <- cos(ph)
    c(th - ph + eps / 2, thd * cp, -ph, thd * cp * (1 - cp))
  }
}

# --- Integrate one walking step ----------------------------------------------
# From post-heel-strike state y0, integrate until the perturbed foot-strike
# condition phi - 2*theta = eps is met. A genuine heel strike is an UPWARD
# crossing (d(phi - 2*theta)/dt > 0) occurring after the early-swing phase
# (t > t_min): mid-swing foot scuffing crosses downward and is ignored
# (Garcia 1998 / Su & Dingwell 2007), and the trivial crossing at step onset
# is excluded by t_min. Upward crossings with theta > 0 correspond to
# recovery ("stumble") steps that catch the walker when the stance leg fails
# to pass the vertical; falls are declared when |theta| exceeds a bound or no
# contact occurs within t_max.
# Samples the trajectory on the global time grid (spacing dt, phase t_offset).
integrate_step <- function(y0, gamma, eps, dt = 0.03, t_offset = 0,
                           t_min = 1.0, t_max = 15,
                           rtol = 1e-10, atol = 1e-12) {
  parms <- list(gamma = gamma)
  rootfun <- function(t, y, parms) {
    c(y[3] - 2 * y[1] - eps,  # foot-strike condition
      abs(y[1]) - 1.2)        # fall guard (stance leg far from vertical)
  }
  t_loc <- 0
  y_cur <- y0
  samples <- NULL
  repeat {
    # global grid points remaining in this step
    grid <- seq(ceiling((t_offset + t_loc - 1e-12) / dt) * dt - t_offset,
                t_max, by = dt)
    grid <- grid[grid > t_loc + 1e-12]
    times <- c(t_loc, grid)
    out <- lsodar(y_cur, times, walker_rhs, parms, rootfunc = rootfun,
                  rtol = rtol, atol = atol)
    troot <- attributes(out)$troot
    n_out <- nrow(out)
    # collect grid samples (exclude initial point and any root row off-grid)
    on_grid <- abs((out[, 1] + t_offset) / dt -
                     round((out[, 1] + t_offset) / dt)) < 1e-9
    keep <- which(on_grid & out[, 1] > t_loc + 1e-12)
    if (length(keep) > 0) samples <- rbind(samples, out[keep, , drop = FALSE])
    if (is.null(troot) || length(troot) == 0) {
      return(list(fell = TRUE, samples = samples, t_end = t_max, y_end = NULL))
    }
    y_root <- out[n_out, 2:5]
    if (abs(y_root[1]) >= 1.2 - 1e-6) {
      return(list(fell = TRUE, samples = samples, t_end = troot, y_end = NULL))
    }
    rate <- y_root[4] - 2 * y_root[2]  # d(phi - 2*theta)/dt at the root
    if (troot > t_min && rate > 0) {
      # genuine heel strike (upward crossing past the early-swing phase)
      return(list(fell = FALSE, samples = samples,
                  t_end = troot, y_end = y_root))
    }
    # spurious root (step onset or scuffing): nudge past it and continue
    nudge <- lsodar(y_root, c(troot, troot + 1e-6), walker_rhs, parms,
                    rtol = rtol, atol = atol)
    y_cur <- nudge[nrow(nudge), 2:5]
    t_loc <- troot + 1e-6
    if (t_loc >= t_max) {
      return(list(fell = TRUE, samples = samples, t_end = t_max, y_end = NULL))
    }
  }
}

# --- Integrate one step on bumpy terrain -------------------------------------
# Physically complete "bumpy ramp" model, all in the FIXED nominal ramp frame
# (dynamics unchanged; terrain only decides when/where contact happens).
# Bump heights h_k (normal to the ramp) at successive contacts; the swing foot
# meets the terrain when its height above the CURRENT pivot equals
# d_k = h_(k+1) - h_k:   cos(theta) - cos(theta - phi) = d_k.
# A genuine contact is a DOWNWARD crossing (foot descending) with the swing
# leg in front (theta < th_guard), past the early-swing phase (t > t_min);
# the mid-swing scuffing passage is ignored (Garcia 1998).
integrate_step_terrain <- function(y0, gamma, d_k, dt = 0.03, t_offset = 0,
                                   t_min = 1.0, th_guard = -0.05, t_max = 15,
                                   rtol = 1e-10, atol = 1e-12) {
  parms <- list(gamma = gamma)
  rootfun <- function(t, y, parms) {
    c(cos(y[1]) - cos(y[1] - y[3]) - d_k,  # swing foot meets terrain
      abs(y[1]) - 1.2)                     # fall guard
  }
  t_loc <- 0
  y_cur <- y0
  samples <- NULL
  repeat {
    grid <- seq(ceiling((t_offset + t_loc - 1e-12) / dt) * dt - t_offset,
                t_max, by = dt)
    grid <- grid[grid > t_loc + 1e-12]
    times <- c(t_loc, grid)
    out <- lsodar(y_cur, times, walker_rhs, parms, rootfunc = rootfun,
                  rtol = rtol, atol = atol)
    troot <- attributes(out)$troot
    n_out <- nrow(out)
    on_grid <- abs((out[, 1] + t_offset) / dt -
                     round((out[, 1] + t_offset) / dt)) < 1e-9
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
    rate <- -sin(th) * thd + sin(th - ph) * (thd - phd)  # d(foot height)/dt
    if (troot > t_min && rate < 0 && th < th_guard) {
      return(list(fell = FALSE, samples = samples,
                  t_end = troot, y_end = y_root))
    }
    nudge <- lsodar(y_root, c(troot, troot + 1e-6), walker_rhs, parms,
                    rtol = rtol, atol = atol)
    y_cur <- nudge[nrow(nudge), 2:5]
    t_loc <- troot + 1e-6
    if (t_loc >= t_max) {
      return(list(fell = TRUE, samples = samples, t_end = t_max, y_end = NULL))
    }
  }
}

# --- Stance-frame -> continuous leg coordinates (Goswami convention) ---------
# theta_G = angle of Leg 1 w.r.t. slope normal, phi_G = Leg 2 relative to Leg 1
# (Su & Dingwell 2007, after Goswami et al. 1997). Continuous across strikes.
to_continuous <- function(mat, stance_leg) {
  th <- mat[, 1]; thd <- mat[, 2]; ph <- mat[, 3]; phd <- mat[, 4]
  if (stance_leg == 1) {
    cbind(thG = th, thGd = thd, phG = -ph, phGd = -phd)
  } else {
    cbind(thG = th - ph, thGd = thd - phd, phG = ph, phGd = phd)
  }
}

# --- Simulate a walking trial ------------------------------------------------
# n_steps steps (2 steps = 1 stride). Starts from y0 (post-heel-strike state).
# variant "terrain" (default): bump heights h_k ~ U(-0.5, 0.5) * rho at the
#   successive contact points of a bumpy ramp (max |h| = rho/2, in units of
#   leg length L); contact via integrate_step_terrain, geometric collision.
# variants "timing"/"geometric"/"full"/"slope": perturbation of the
#   foot-strike condition eps ~ U(-0.5, 0.5) * rho (Su & Dingwell 2007, Eq. 4)
#   under the corresponding reading (see heelstrike_map).
# Returns global-grid samples in continuous coordinates, per-step events, and
# a fell flag.
simulate_walker <- function(n_steps, gamma, rho, y0, dt = 0.03, seed = NULL,
                            t_min = 1.0, variant = "terrain",
                            eps_seq = NULL) {
  if (!is.null(seed)) set.seed(seed)
  if (!is.null(eps_seq)) {
    # externally supplied perturbation sequence (e.g., ARFIMA-structured);
    # rho is ignored, the sequence is used as-is under the given variant
    stopifnot(length(eps_seq) >= n_steps)
    eps_all <- eps_seq[seq_len(n_steps)]
  } else if (variant == "terrain") {
    h_all <- runif(n_steps + 1, -0.5, 0.5) * rho  # bump heights (units of L)
    eps_all <- diff(h_all)                        # d_k = h_(k+1) - h_k
  } else {
    eps_all <- runif(n_steps, -0.5, 0.5) * rho
  }
  t_glob <- 0
  y_cur <- y0
  stance <- 1L
  samp_list <- vector("list", n_steps)
  events <- data.frame(step = integer(0), t_start = numeric(0),
                       t_end = numeric(0), duration = numeric(0),
                       eps = numeric(0), stance = integer(0))
  bounds <- vector("list", n_steps)  # continuous-coord boundary states
  fell <- FALSE
  for (k in seq_len(n_steps)) {
    if (variant == "terrain") {
      st <- integrate_step_terrain(y_cur, gamma, eps_all[k], dt = dt,
                                   t_offset = t_glob, t_min = t_min)
    } else if (variant == "slope") {
      # Su & Dingwell 2007 equivalence: perturbing the foot-strike condition
      # by eps equals walking that step on slope gamma_k = gamma - eps/2
      # (their Eq. for gamma_pert reduces to -eps/2). Perturbation acts
      # through gravity during the whole step; strike condition unperturbed.
      st <- integrate_step(y_cur, gamma - eps_all[k] / 2, eps = 0, dt = dt,
                           t_offset = t_glob, t_min = t_min)
    } else {
      st <- integrate_step(y_cur, gamma, eps_all[k], dt = dt,
                           t_offset = t_glob, t_min = t_min)
    }
    y_start_cont <- to_continuous(matrix(y_cur, 1), stance)
    if (!is.null(st$samples)) {
      sm <- st$samples
      cont <- to_continuous(sm[, 2:5, drop = FALSE], stance)
      samp_list[[k]] <- cbind(t = sm[, 1] + t_glob, cont, step = k)
    }
    if (st$fell) { fell <- TRUE; break }
    y_end_cont <- to_continuous(matrix(st$y_end, 1), stance)
    events <- rbind(events, data.frame(
      step = k, t_start = t_glob, t_end = t_glob + st$t_end,
      duration = st$t_end, eps = eps_all[k], stance = stance))
    bounds[[k]] <- list(start = y_start_cont, end = y_end_cont)
    map_variant <- if (variant %in% c("terrain", "slope")) "geometric" else
      variant
    y_cur <- heelstrike_map(st$y_end, variant = map_variant, eps = eps_all[k])
    t_glob <- t_glob + st$t_end
    stance <- 3L - stance
  }
  samples <- do.call(rbind, samp_list[!vapply(samp_list, is.null, TRUE)])
  list(samples = samples, events = events, bounds = bounds,
       fell = fell, n_steps_done = nrow(events), dt = dt, rho = rho)
}

# --- Fixed point of the step-to-step map (period-1 gait) ---------------------
# Solves for post-heel-strike (theta, theta_dot) mapped onto itself after one
# step. Newton iteration with numerical Jacobian. Also returns the step period
# and the eigenvalues of the linearized step map (deterministic validation
# against Garcia et al. 1998).
step_map <- function(x, gamma) {
  y0 <- c(x[1], x[2], 2 * x[1], x[2] * (1 - cos(2 * x[1])))
  st <- integrate_step(y0, gamma, eps = 0, dt = 1)  # sampling irrelevant here
  if (st$fell) return(list(x_next = c(NA, NA), period = NA))
  y_new <- heelstrike_map(st$y_end)
  list(x_next = y_new[1:2], period = st$t_end)
}

find_fixed_point <- function(gamma, x_init = c(0.2, -0.2),
                             tol = 1e-12, max_iter = 50) {
  x <- x_init
  for (i in seq_len(max_iter)) {
    f0 <- step_map(x, gamma)$x_next - x
    if (any(is.na(f0))) stop("step map failed during Newton iteration")
    if (sqrt(sum(f0^2)) < tol) break
    h <- 1e-7
    jac <- matrix(0, 2, 2)
    for (j in 1:2) {
      xp <- x; xp[j] <- xp[j] + h
      fj <- step_map(xp, gamma)$x_next - xp  # f(x + h e_j)
      jac[, j] <- (fj - f0) / h              # d(F(x) - x)/dx
    }
    x <- x - solve(jac, f0)
  }
  sm <- step_map(x, gamma)
  # linearized step map S: x_{k+1} = S x_k (around fixed point)
  h <- 1e-7
  S <- matrix(0, 2, 2)
  for (j in 1:2) {
    xp <- x; xp[j] <- xp[j] + h
    S[, j] <- (step_map(xp, gamma)$x_next - sm$x_next) / h
  }
  list(theta = x[1], theta_dot = x[2], period = sm$period,
       eig_step_map = eigen(S)$values,
       y0 = c(x[1], x[2], 2 * x[1], x[2] * (1 - cos(2 * x[1]))))
}

# --- Time-normalization of strides (Su & Dingwell 2007) ----------------------
# Each stride = 2 consecutive steps; each step normalized separately to 0-50%
# and 50-100% so foot strikes occur at fixed normalized times. 101 samples per
# stride. Interpolation uses within-step samples plus exact boundary states.
# Returns array [n_strides, 101, 4].
normalize_strides <- function(sim, n_half = 50) {
  ev <- sim$events
  n_strides <- floor(nrow(ev) / 2)
  vars <- c("thG", "thGd", "phG", "phGd")
  arr <- array(NA_real_, dim = c(n_strides, 2 * n_half + 1, 4),
               dimnames = list(NULL, NULL, vars))
  smp <- sim$samples
  idx_by_step <- split(seq_len(nrow(smp)), smp[, "step"])
  for (s in seq_len(n_strides)) {
    for (half in 1:2) {
      k <- 2 * (s - 1) + half
      t0 <- ev$t_start[k]; t1 <- ev$t_end[k]
      idx <- idx_by_step[[as.character(k)]]
      idx <- idx[smp[idx, "t"] > t0 & smp[idx, "t"] < t1]
      tt <- c(t0, smp[idx, "t"], t1)
      pct_target <- seq(t0, t1, length.out = n_half + 1)
      for (v in 1:4) {
        yy <- c(sim$bounds[[k]]$start[1, v], smp[idx, 1 + v],
                sim$bounds[[k]]$end[1, v])
        yi <- approx(tt, yy, xout = pct_target, ties = "ordered")$y
        cols <- if (half == 1) 1:(n_half + 1) else (n_half + 1):(2 * n_half + 1)
        arr[s, cols, v] <- yi
      }
    }
  }
  arr
}

# --- MeanSD (Su & Dingwell 2007, Eq. 5) --------------------------------------
# SD across strides at each normalized time, averaged over the stride.
mean_sd <- function(arr) {
  apply(arr, 3, function(m) mean(apply(m, 2, sd)))
}

# --- Orbital stability: max Floquet multipliers (Su & Dingwell 2007) ---------
# One Poincare section per normalized time; fixed point x* = mean unperturbed
# trajectory; Jacobian estimated by least squares over stride-to-stride pairs.
floquet_multipliers <- function(arr, xstar,
                                sections = seq(1, 101, by = 1)) {
  n_str <- dim(arr)[1]
  vapply(sections, function(sec) {
    X <- arr[, sec, ] - matrix(xstar[sec, ], n_str, 4, byrow = TRUE)
    Z <- X[-n_str, , drop = FALSE]
    Y <- X[-1, , drop = FALSE]
    # pseudo-inverse least squares: post-strike sections have exactly
    # collinear states (phi_G = -2 theta_G), making plain LS singular
    B <- tryCatch(MASS::ginv(Z) %*% Y, error = function(e) NULL)
    if (is.null(B)) return(NA_real_)
    max(Mod(eigen(t(B), only.values = TRUE)$values))
  }, numeric(1))
}

# --- ARFIMA(0,d,0) series, exact Gaussian simulation -------------------------
# Autocovariance recursion rho(k) = rho(k-1) * (k-1+d)/(k-d), rho(0) = 1
# (Hosking 1981), Cholesky factorization of the Toeplitz covariance matrix.
# The series is z-scored and rescaled so every realization has EXACTLY mean 0
# and standard deviation sd_target: only the correlation structure differs
# across d conditions (amplitude control). d = 0 yields white Gaussian noise.
arfima_series <- function(n, d, sd_target = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  z <- rnorm(n)
  if (d != 0) {
    rho <- numeric(n)
    rho[1] <- 1
    for (k in 1:(n - 1)) rho[k + 1] <- rho[k] * (k - 1 + d) / (k - d)
    L <- chol(toeplitz(rho))
    z <- as.numeric(t(L) %*% z)
  }
  (z - mean(z)) / sd(z) * sd_target
}

# --- Detrended fluctuation analysis (DFA-1) ----------------------------------
# Standard first-order DFA (Peng et al. 1994): integrated profile, linear
# detrending in non-overlapping windows, log-log slope alpha over window sizes
# n_min..N/4 (log-spaced). alpha = 0.5 white noise, ~1 for 1/f.
dfa_alpha <- function(x, n_min = 4) {
  N <- length(x)
  y <- cumsum(x - mean(x))
  n_max <- floor(N / 4)
  sizes <- unique(round(exp(seq(log(n_min), log(n_max), length.out = 20))))
  fn <- vapply(sizes, function(n) {
    n_win <- floor(N / n)
    idx <- seq_len(n_win * n)
    seg <- matrix(y[idx], nrow = n)
    t_lin <- seq_len(n)
    detr <- apply(seg, 2, function(s) resid(lm.fit(cbind(1, t_lin), s)))
    sqrt(mean(detr^2))
  }, numeric(1))
  fit <- lm(log(fn) ~ log(sizes))
  list(alpha = unname(coef(fit)[2]), sizes = sizes, fn = fn)
}

# --- Local divergence exponents (Rosenstein; Su & Dingwell 2007) -------------
# The mean log divergence (MLD) curve is computed by the Rosenstein engine of
# the RACING package, used UNMODIFIED: scripts/racing/rosenstein_divergence.R,
# release v1.0.2 (provenance and checksums in scripts/racing/README.md).
#   Terrier P (2026). RACING: R-based Attractor Complexity INdex for Gait
#   (v1.0.2). Zenodo. https://doi.org/10.5281/zenodo.20542521
#   Terrier P (2026). SoftwareX 34:102705.
#   https://doi.org/10.1016/j.softx.2026.102705
# RACING's entry point, rosenstein_divergence(), delay-embeds a univariate
# signal. Here the 4D model state [thG, thGd, phG, phGd] is observed directly
# (no embedding, as in Su & Dingwell 2007), so two of its documented steps are
# called on that matrix:
#   find_nearest_neighbors(): every sample is a reference; its nearest neighbor
#     is searched outside a Theiler window of one stride (RANN KD-tree);
#   compute_divergence(): mean log distance after k = 1..horizon samples, over
#     the pairs whose two trajectories still exist at k (the pair set shrinks
#     slightly as k grows); zero distances are excluded.
# SIM-ACI adds the initial separation (k = 0) under the same rules, then applies
# its own slope estimation: 7-sample Hamming zero-lag smoothing, 3-point
# instantaneous slopes, lambda_S = mean slope over 0-0.1 step, lambda_L = mean
# slope over 3-4 steps (Su & Dingwell 2007).

.racing_md5 <- "29940702bbd3f155023274cf62e8cc2f"  # v1.0.2, any line endings

# Load the vendored RACING file into its own environment, once, when this file
# is sourced. The path is resolved relative to this file.
racing_engine <- local({
  src <- Filter(is.character, lapply(sys.frames(), function(f) f$ofile))
  base <- if (length(src) > 0) dirname(normalizePath(src[[length(src)]])) else
    get0("script_dir", envir = globalenv(), ifnotfound = ".")
  path <- file.path(base, "racing", "rosenstein_divergence.R")
  if (!file.exists(path)) stop("RACING engine not found: ", path)
  # Without RANN, RACING switches to an exact but pure-R O(N^2) search: same
  # neighbors, several minutes per trial. Require it.
  if (!requireNamespace("RANN", quietly = TRUE)) {
    stop("package 'RANN' is required by the RACING neighbor search")
  }
  md5 <- unname(tools::md5sum(bytes = charToRaw(
    paste(readLines(path, warn = FALSE), collapse = "\n"))))
  if (!identical(md5, .racing_md5)) {
    warning("RACING engine differs from release v1.0.2: ", path)
  }
  env <- new.env(parent = globalenv())
  sys.source(path, envir = env, keep.source = FALSE)
  env
})

divergence_exponents <- function(sim, tau_step) {
  X <- unname(as.matrix(sim$samples[, c("thG", "thGd", "phG", "phGd")]))
  dt <- sim$dt
  N <- nrow(X)
  n_hor <- round(4 * tau_step / dt)    # horizon: 4 steps (samples)
  theiler <- round(2 * tau_step / dt)  # Theiler window: one stride (samples)
  nbr <- racing_engine$find_nearest_neighbors(X, mean_period = theiler)
  mld_k <- racing_engine$compute_divergence(X, nbr, max_iter = n_hor)
  # k = 0: initial separation of every pair, zero distances excluded
  d0 <- sqrt(rowSums((X - X[nbr, , drop = FALSE])^2))
  mld <- c(mean(log(d0[d0 > 0])), mld_k)  # mld[i + 1] = after i samples
  # 7-sample Hamming zero-lag moving average
  w <- 0.54 - 0.46 * cos(2 * pi * (0:6) / 6)
  w <- w / sum(w)
  mld_f <- as.numeric(stats::filter(mld, w, sides = 2))
  # instantaneous slopes, 3-point difference (Su & Dingwell 2007, Eq. 12)
  slopes <- rep(NA_real_, n_hor + 1)
  for (i in 2:n_hor) slopes[i] <- (mld_f[i + 1] - mld_f[i - 1]) / (2 * dt)
  i_short <- which(!is.na(slopes) & (0:n_hor) * dt <= 0.1 * tau_step)
  i_long  <- which(!is.na(slopes) & (0:n_hor) * dt >= 3 * tau_step &
                     (0:n_hor) * dt <= 4 * tau_step)
  list(mld = mld, mld_smooth = mld_f, slopes = slopes,
       time = (0:n_hor) * dt,
       # pairs present over the whole horizon (RACING's set shrinks with k)
       n_pairs = sum(seq_len(N) <= N - n_hor & nbr <= N - n_hor),
       lambda_s = mean(slopes[i_short]), lambda_l = mean(slopes[i_long]))
}
