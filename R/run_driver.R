# Title: Run driver: simulation of a run, trimming, redraws, seed rule and campaign log
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model; trim_run() repeats trim_sim() of feasibility/scripts/02 and 04
# Date: 2026-11-05
# Description: specification, sections 8 and 10. Seed rule approved by the author on
#   2026-11-05 (open choice O4): s = master + 100,000 x attempt + 100 x cell + replicate.
#   walkbench 0.3.0 (2026-11-06): transient_cycles() gives the length of the transient under
#   the rule that the author chose on 2026-11-06 at 13:55 (decision register of the SIM-ACI project;
#   analysis plan, draft 0.5, sections 4.3 and 4.8), and it is the default of run_trial().
#   walkbench 0.4.0 (2026-11-06): rules of the analysis plan frozen at commit ba72afe
#   (osf.io/m74tk), section 4.3: transient of a closed loop from the eigenvalues of the
#   linearized loop at the gain of the cell, with the transient of the exogenous injection at a
#   gain of zero (transient_cycles_loop()); cells and replicates of a campaign numbered from 1
#   (check in seed_campaign()); DFA of the plan passed to estimate_all() by run_trial().
#   walkbench 0.6.0 (2026-11-06): numerical failures of the integrator, distinct from falls,
#   under section 4 of update 1 of the registration osf.io/m74tk (run_trial()).
#   walkbench 0.6.1 (2026-11-07): only an error of the compiled solver of deSolve counts as a
#   numerical failure (from_integrator()); any other error stops run_trial() with its original
#   message; the warnings of the solver are kept. Task of the coordination session (Claude
#   Opus 5.5) after the answer of the author of 2026-11-07 at 07:31 (decision
#   register of the SIM-ACI project).
#   walkbench 0.6.2 (2026-11-07): fixes of the review of the code of campaigns 2 and 3 of M1 by
#   a verification sub-agent (Claude Fable 5.1), triage approved by the author on 2026-11-07
#   at 08:41 (register; SIMACI/docs/reviews/2026-11-07_triage_campaigns_m1.md): F1, an attempt
#   that ends as a fall after a warning of the solver is a numerical failure (option B of the
#   author, 08:17); F4, the summary row of a trial without outcomes has the outcome columns,
#   empty; F2, check_output_folder(); F7 and F10, run log; F8, documented risk.
# Dependencies: stats, utils, jsonlite, deSolve; git (optional, for the run log)
# Input: none
# Output: campaign_log.json and runs.csv, written by write_campaign_log()

#' Simulate one run of a model
#'
#' @param model a `simaci_model`.
#' @param input a `simaci_input` (open loop); for a closed loop, the exogenous series is
#'   held by the controller and `input` may be `NULL`.
#' @param n_cycles number of cycles to simulate.
#' @param y0 full initial state; by default the post-contact state of the fixed point.
#' @param controller a controller function, or `NULL`.
#' @param metronome a `simaci_metronome`, or `NULL`.
#' @param injection injection point.
#' @return A list of class `simaci_run` (specification, section 8.1).
#' @export
simulate_run <- function(model, input = NULL, n_cycles, y0 = NULL, controller = NULL,
                         metronome = NULL, injection = model$default_injection) {
  if (!inherits(model, "simaci_model")) stop("model must be a simaci_model")
  if (is.null(controller) && is.null(input)) stop("an open-loop run needs an input")
  if (!is.null(input)) check_input(input, model, n_cycles)
  if (is.null(y0)) y0 <- model$fixed_point()$y0
  t0 <- Sys.time()
  out <- model$simulate(input = input, n_cycles = n_cycles, y0 = y0,
                        controller = controller, metronome = metronome,
                        injection = injection)
  structure(c(list(model = model$id, injection = injection), out,
              list(dt = model$params$dt, input = input,
                   metronome_period = if (is.null(metronome)) NA_real_ else metronome$period,
                   n_redraws = NA_integer_, seeds = NA_real_, n_transient = 0L,
                   meta = list(interface_version = as.character(utils::packageVersion("walkbench")),
                               start = format(t0, "%Y-%m-%dT%H:%M:%S%z"),
                               wall_time_s = as.numeric(difftime(Sys.time(), t0, units = "secs"))))),
            class = "simaci_run")
}

#' Remove the transient cycles of a run
#'
#' Same operations as `trim_sim()` of the feasibility scripts 02 and 04: the kept cycles are
#' renumbered from 1 and their times are rebased on the start of the first kept cycle.
#'
#' @param run a `simaci_run` without fall.
#' @param n_transient number of cycles to remove.
#' @return The trimmed `simaci_run`.
#' @export
trim_run <- function(run, n_transient) {
  if (run$fell) stop("a run that fell is not trimmed")
  n_keep <- run$n_cycles_done - n_transient
  keep <- (n_transient + 1L):(n_transient + n_keep)
  ev <- run$events
  t0 <- ev$t_start[keep[1]]
  smp <- run$samples
  smp <- smp[smp[, "cycle"] %in% keep, , drop = FALSE]
  smp[, "t"] <- smp[, "t"] - t0
  smp[, "cycle"] <- smp[, "cycle"] - n_transient
  ev <- ev[keep, , drop = FALSE]
  ev$t_start <- ev$t_start - t0
  ev$t_end <- ev$t_end - t0
  ev$cycle <- ev$cycle - n_transient
  run$samples <- smp
  run$events <- ev
  run$bounds <- run$bounds[keep]
  run$n_cycles_done <- n_keep
  run$n_transient <- n_transient
  run
}

#' Number of transient cycles of a model
#'
#' Rule of the SIM-ACI analysis plan (draft 0.5, sections 4.3 and 4.8), chosen by the author
#' on 2026-11-06: the smallest number of cycles n such that lambda^(2n) <= `tol`, with a
#' minimum of `minimum` cycles. lambda is the largest modulus of the eigenvalues of the cycle
#' map at the fixed point (`model$fixed_point()$eig`). For a linear response that starts at
#' the fixed point, lambda^(2n) is the fraction of the stationary variance still missing after
#' n cycles. The rule gives 20 cycles for M1 (lambda = 0.589) and 34 cycles for M2 with
#' r_f = 0.45 and k = 5 (lambda = 0.923).
#'
#' @param model a `simaci_model`.
#' @param tol largest fraction of the stationary variance missing at the end of the transient.
#' @param minimum smallest number of transient cycles.
#' @return The number of transient cycles, an integer.
#' @seealso [transient_cycles_loop()] for the closed loops of variants H3a and H3b.
#' @export
transient_cycles <- function(model, tol = 0.005, minimum = 20L) {
  transient_from_radius(max(Mod(model$fixed_point()$eig)), tol, minimum)
}

#' Number of transient cycles for a given largest eigenvalue modulus
#'
#' The smallest n such that `mu^(2n) <= tol`, with a minimum of `minimum` cycles (analysis
#' plan, section 4.3).
#'
#' @param mu largest modulus of the eigenvalues of the linearized cycle map, open or closed
#'   loop.
#' @inheritParams transient_cycles
#' @return The number of transient cycles, an integer.
#' @export
transient_from_radius <- function(mu, tol = 0.005, minimum = 20L) {
  if (!is.finite(mu) || mu >= 1) {
    stop(sprintf("the loop is not stable (largest eigenvalue modulus %.6f)", mu))
  }
  n <- if (mu == 0) 0L else max(0L, as.integer(ceiling(log(tol) / (2 * log(mu)))))
  # the smallest n with mu^(2n) <= tol, whatever the rounding of the logarithms
  while (n > 0L && mu^(2 * (n - 1L)) <= tol) n <- n - 1L
  while (mu^(2 * n) > tol) n <- n + 1L
  max(as.integer(minimum), n)
}

#' Number of transient cycles under a controller
#'
#' Rule of the analysis plan frozen at commit ba72afe (section 4.3; decision of the author of
#' 2026-11-06 at 17:55): mu is the largest modulus of the eigenvalues of the linearized closed
#' loop at the gain of the cell ([closed_loop_radius()]). At a gain of zero the controller is
#' off: in variant H3b the loop keeps the eigenvalue 1 of the uncorrected asynchrony, and the
#' transient is that of the exogenous injection, from the open-loop eigenvalues. In M1 the rule
#' gives 52 cycles at kappa = 0.05 (static scale); in M2 with r_f = 0.45 and k = 5 it gives 34
#' to 171 cycles over the eight levels of g from 0 to 0.10 (deadbeat scale).
#'
#' @inheritParams closed_loop_matrix
#' @inheritParams transient_cycles
#' @return The number of transient cycles, an integer.
#' @export
transient_cycles_loop <- function(lin, controller = c("h3a", "h3b"), scale, gain,
                                  corrected_index = 1L, tol = 0.005, minimum = 20L) {
  controller <- match.arg(controller)
  if (length(gain) != 1L || !is.finite(gain) || gain < 0) stop("gain must be one value >= 0")
  mu <- if (gain == 0) max(Mod(lin$eig)) else
    closed_loop_radius(lin, controller, scale, gain, corrected_index)
  transient_from_radius(mu, tol, minimum)
}

#' Seed of a run under the campaign rule of SIM-ACI
#'
#' `s = master + 100,000 * attempt + 100 * cell + replicate`, with cell in 0 to 999,
#' replicate in 0 to 99 and attempt in 0 to 20 (21 attempts at most, analysis plan, section
#' 4.4). For the master seed of a campaign ([campaign_master()], 100,000,000 or more), the
#' cells and the replicates are numbered from 1 (analysis plan frozen at commit ba72afe,
#' section 4.3). The calibration runs, with master seeds below 100,000,000, numbered them
#' from 0.
#'
#' @param master master seed of the campaign.
#' @param cell index of the cell.
#' @param replicate index of the replicate.
#' @param attempt attempt number (0 for the first draw).
#' @return The seed, an integer below 2^31 - 1.
#' @export
seed_campaign <- function(master, cell, replicate, attempt = 0L) {
  if (any(cell < 0 | cell > 999)) stop("cell must lie in 0 to 999")
  if (any(replicate < 0 | replicate > 99)) stop("replicate must lie in 0 to 99")
  if (any(attempt < 0 | attempt > 20)) stop("attempt must lie in 0 to 20")
  if (any(master >= 1e8)) {
    if (!all(master %in% campaign_table()$master)) {
      stop("a master seed of 100,000,000 or more must be that of a campaign (campaign_master())")
    }
    if (any(cell < 1 | replicate < 1)) stop("in a campaign, cells and replicates are numbered from 1")
  }
  s <- master + 100000 * attempt + 100 * cell + replicate
  if (any(master < 1 | s > .Machine$integer.max)) stop("seed outside 1 to 2^31 - 1")
  as.integer(s)
}

#' Check that the master seeds of several campaigns cannot share a seed
#'
#' @param masters master seeds of the campaigns.
#' @return `TRUE` when every pair differs by at least 2,100,000.
#' @export
check_master_seeds <- function(masters) {
  if (length(masters) < 2L) return(TRUE)
  all(diff(sort(masters)) >= 2100000)
}

#' Draw function under the single-seed rule
#'
#' After `set.seed(seed)`, the input is drawn first, then the initial jitter of the
#' post-contact state (specification, section 10.2).
#'
#' @param model a `simaci_model`.
#' @param make_input generator `function(n, seed)`; it is called with `seed = NULL`.
#' @param n number of values of the input.
#' @param jitter_sd standard deviation of the jitter added to each state variable.
#' @return A function `function(seed)` that returns `list(input, y0)`.
#' @export
draw_single_seed <- function(model, make_input, n, jitter_sd = 1e-9) {
  function(seed) {
    set.seed(seed)
    input <- make_input(n, NULL)
    y0 <- model$fixed_point()$y0
    list(input = input, y0 = y0 + stats::rnorm(length(y0)) * jitter_sd)
  }
}

# Whether the compiled solver of deSolve signaled a condition (walkbench 0.6.1). Called from a
# calling handler, with the number of the frame of the handler (sys.nframe()). The frame of
# origin is the innermost frame whose call is the call of the condition. The condition comes
# from the solver when the function of that frame belongs to the namespace of deSolve and no
# call to stop() or warning() lies between that frame and the handler. Compiled code of
# deSolve then raised it. The class of the condition cannot serve, since deSolve signals
# simple conditions, and its message changes with the version of deSolve. The call alone
# cannot serve either: the error "could not find function "lsodar"" has the call lsodar(...),
# but the function never ran, so no frame has that call.
from_integrator <- function(cond, n_handler) {
  call <- conditionCall(cond)
  if (is.null(call) || !isNamespaceLoaded("deSolve")) return(FALSE)
  ns <- asNamespace("deSolve")
  signalers <- list(base::stop, base::warning)
  for (i in rev(seq_len(n_handler - 1L))) {
    if (!identical(sys.call(i), call)) next
    f <- sys.function(i)
    if (is.primitive(f) || !identical(topenv(environment(f)), ns)) return(FALSE)
    above <- if (i + 1L < n_handler) seq.int(i + 1L, n_handler - 1L) else integer(0)
    for (j in above) {
      g <- sys.function(j)
      if (any(vapply(signalers, identical, logical(1), g))) return(FALSE)
    }
    return(TRUE)
  }
  FALSE
}

#' One trial: draws, redraws after a fall, trimming and outcomes
#'
#' @param model a `simaci_model`.
#' @param draw function `function(seed)` that returns `list(input, y0)`.
#' @param seed_of function `function(attempt)` that returns the seed of an attempt.
#' @param n_transient,n_keep numbers of transient and kept cycles; by default, the number
#'   of transient cycles follows [transient_cycles()] (20 for M1).
#' @param controller_factory `NULL`, or a function `function(input)` that returns a
#'   controller; the drawn input is then its exogenous series.
#' @param metronome a `simaci_metronome`, or `NULL`.
#' @param injection injection point.
#' @param max_redraws largest number of redraws after falls.
#' @param settings estimation settings.
#' @param xstar reference stride for the Floquet multipliers, or `NULL`.
#' @param cell,replicate identifiers written in the summary row.
#' @param keep_run keep the trimmed run in the result.
#' @param dfa settings of the DFA, from [dfa_settings()]; by default the configuration of the
#'   analysis plan. The replay of the feasibility study passes `dfa_settings("feasibility")`.
#' @return A list with `summary` (one row), `divergence`, `seeds`, `n_redraws`, `fell`,
#'   `n_falls`, `n_numerical_failures`, `numerical_failures` (the messages of the
#'   integrator), `integrator_warnings` (the warnings of the solver, each with its attempt)
#'   and, on request, `run`. With the default `max_redraws = 20`, a trial makes 21 attempts
#'   at most (analysis plan, section 4.4).
#'
#'   Numerical failures follow section 4 of update 1 of the registration osf.io/m74tk. An
#'   attempt in which the integrator stops on an error is a numerical failure, distinct from a
#'   fall, during the transient or after it. With an input that can be redrawn, the next
#'   attempt draws a new input, and the failures count among the 21 attempts. With a fixed
#'   input (a real stride series or a surrogate), the trial ends, as after a fall
#'   (section 4.8). A trial that ends without outcomes, after falls or failures at all its
#'   attempts or on a fixed input, returns `fell = TRUE` and a summary row without outcomes;
#'   `n_falls` and `n_numerical_failures` give the cause. Since walkbench 0.6.2, that row has
#'   the outcome columns of [estimate_all()], empty, so that every summary row of a model has
#'   the same columns.
#'
#'   Since walkbench 0.6.1, an error of [simulate_run()] counts as an error of the integrator
#'   only when the compiled solver of deSolve raised it. The frame of origin is the innermost
#'   frame whose call is the call of the condition. When the condition is signaled, that frame
#'   must run a function of the namespace of deSolve, and no call to `stop()` may lie between
#'   that frame and the handler. Any other error stops the trial, and so the campaign, with
#'   its original message and call. Such errors include the errors of the model code and of a
#'   controller, and those of the right-hand side and of the root function. They also include
#'   an argument check of the R code of deSolve. A function that is not found stops the trial
#'   too, such as `lsodar()` on an R process where deSolve is not attached.
#'
#'   With deSolve 1.42, the compiled solver raises an error when it rejects its input before
#'   the first step (`istate = -3`). It reports its other error returns as warnings and
#'   returns a truncated output. The integration loops of M1 and M2 then find no foot strike
#'   and record a fall, although the integration stopped before the time limit of the fall
#'   rule. Since walkbench 0.6.2, an attempt counts as a numerical failure when it ends as a
#'   fall after at least one warning of the solver. This is the definition of section 4 of
#'   update 1 (decision of the author of SIM-ACI, 2026-11-07). The warnings are read with the
#'   criterion of the errors. An attempt that completes keeps its outcomes. The warnings
#'   of every attempt stay in `integrator_warnings`.
#'
#'   Known risk, accepted by the author of SIM-ACI (review of 2026-11-07, finding F8). In the
#'   pinned `integrate_step()` of M1, a spurious root may leave the integration within one
#'   sampling step below the time limit of 15 time units. The next call of `seq()` then
#'   raises the R error "wrong sign in 'by' argument". The solver did not raise it, so it
#'   stops the trial and the campaign. The inputs and the seeds are deterministic, so the same
#'   simulation would stop again; the author then decides how the attempt is classified.
#' @export
run_trial <- function(model, draw, seed_of, n_transient = transient_cycles(model), n_keep,
                      controller_factory = NULL, metronome = NULL,
                      injection = model$default_injection, max_redraws = 20L,
                      settings = model$estimation_settings, xstar = NULL, cell = NA,
                      replicate = NA, keep_run = FALSE, dfa = dfa_settings()) {
  n_total <- n_transient + n_keep
  seeds <- numeric(0)
  run <- NULL
  n_falls <- 0L
  n_failures <- 0L
  messages <- character(0)
  integrator_warnings <- character(0)
  completed <- FALSE
  t0 <- Sys.time()
  for (a in 0:max_redraws) {
    s <- seed_of(a)
    seeds <- c(seeds, s)
    d <- draw(s)
    ctrl <- if (is.null(controller_factory)) NULL else controller_factory(d$input)
    # flags of this attempt, set by the handlers while a condition is signaled
    solver_error <- FALSE
    solver_warned <- FALSE
    attempt_warnings <- character(0)
    run <- tryCatch(
      withCallingHandlers(
        simulate_run(model, input = d$input, n_cycles = n_total, y0 = d$y0,
                     controller = ctrl, metronome = metronome, injection = injection),
        # the origin is read while the condition is signaled, before the stack unwinds
        error = function(e) {
          n <- sys.nframe()
          solver_error <<- isTRUE(tryCatch(from_integrator(e, n), error = function(e2) FALSE))
        },
        warning = function(w) {
          n <- sys.nframe()
          if (isTRUE(tryCatch(from_integrator(w, n), error = function(e2) FALSE))) {
            solver_warned <<- TRUE
            attempt_warnings <<- c(attempt_warnings, conditionMessage(w))
          }
        }),
      error = function(e) e)
    if (length(attempt_warnings) > 0L) {
      integrator_warnings <- c(integrator_warnings, sprintf("attempt %d: %s", a, attempt_warnings))
    }
    if (inherits(run, "error")) {
      # an error that the solver of deSolve did not raise stops the trial with its original
      # message and call (walkbench 0.6.1)
      if (!solver_error) stop(run)
      n_failures <- n_failures + 1L
      messages <- c(messages, conditionMessage(run))
    } else if (!run$fell && run$n_cycles_done >= n_total) {
      completed <- TRUE
      break
    } else if (solver_warned) {
      # the solver stopped on an error return that deSolve reports as a warning: the step
      # ended before the time limit of the fall rule, without a fall (walkbench 0.6.2)
      n_failures <- n_failures + 1L
      messages <- c(messages, paste(attempt_warnings, collapse = "; "))
    } else {
      n_falls <- n_falls + 1L
    }
    if (!isTRUE(d$input$redrawable)) break
  }
  n_redraws <- length(seeds) - 1L
  wall <- function() as.numeric(difftime(Sys.time(), t0, units = "secs"))
  if (!completed) {
    # the columns of a completed trial, with empty outcomes (walkbench 0.6.2)
    row <- cbind(data.frame(model = model$id, injection = injection, cell = cell,
                            replicate = replicate, n_redraws = n_redraws, fell = TRUE),
                 outcomes_na(model))
    row$n_falls <- n_falls
    row$n_numerical_failures <- n_failures
    return(list(summary = row, divergence = NULL, seeds = seeds, n_redraws = n_redraws,
                fell = TRUE, n_falls = n_falls, n_numerical_failures = n_failures,
                numerical_failures = messages, integrator_warnings = integrator_warnings,
                wall_time_s = wall()))
  }
  run <- trim_run(run, n_transient)
  run$n_redraws <- n_redraws
  run$seeds <- seeds
  row <- estimate_all(run, model, settings, xstar = xstar, input = d$input, cell = cell,
                      replicate = replicate, dfa = dfa)
  dv <- attr(row, "divergence")
  attr(row, "divergence") <- NULL
  row$n_falls <- n_falls
  row$n_numerical_failures <- n_failures
  out <- list(summary = row, divergence = dv, seeds = seeds, n_redraws = n_redraws,
              fell = FALSE, n_falls = n_falls, n_numerical_failures = n_failures,
              numerical_failures = messages, integrator_warnings = integrator_warnings,
              wall_time_s = wall())
  if (keep_run) out$run <- run
  out
}

# Git executable: option walkbench.git, environment variable WALKBENCH_GIT, or the PATH
find_git <- function() {
  g <- getOption("walkbench.git", Sys.getenv("WALKBENCH_GIT", unset = ""))
  if (!nzchar(g)) g <- unname(Sys.which("git"))
  if (nzchar(g) && file.exists(g)) g else NA_character_
}

# Commit and status of the repository that holds `path`: the status of the folder `path`, or
# of the whole repository with `whole = TRUE` (walkbench 0.6.2)
git_state <- function(path, whole = FALSE) {
  g <- find_git()
  if (is.na(g) || is.null(path)) return(list(commit = NA_character_, modified = NA_character_))
  qp <- paste0('"', normalizePath(path, winslash = "/"), '"')
  commit <- tryCatch(suppressWarnings(system2(g, c("-C", qp, "rev-parse", "HEAD"),
                                               stdout = TRUE, stderr = FALSE)),
                     error = function(e) NA_character_)
  scope <- if (isTRUE(whole)) character(0) else c("--", ".")
  status <- tryCatch(suppressWarnings(system2(g, c("-C", qp, "status", "--porcelain", scope),
                                               stdout = TRUE, stderr = FALSE)),
                     error = function(e) NA_character_)
  list(commit = if (length(commit) == 0L) NA_character_ else commit[1],
       modified = if (length(status) == 0L) character(0) else status)
}

# Absolute form of a path that may not exist yet: the deepest existing folder of the path is
# normalized, and the missing parts are appended
absolute_path <- function(p) {
  p <- gsub("\\\\", "/", p)
  rest <- character(0)
  while (!file.exists(p)) {
    parent <- dirname(p)
    if (identical(parent, p)) break
    rest <- c(basename(p), rest)
    p <- parent
  }
  base <- normalizePath(p, winslash = "/", mustWork = FALSE)
  out <- paste(c(base, rest), collapse = "/")
  out <- gsub("/+", "/", out)
  if (grepl("^[A-Za-z]:/$", out)) out else sub("/$", "", out)
}

#' Check the output folder of a campaign before any file is written
#'
#' Stops when `dir` is an existing folder that holds a file or a folder, unless `overwrite`
#' is `TRUE`, and when `dir` is a file. With `forbidden_root`, stops also when `dir` lies
#' inside that folder or is that folder. The smoke checks of the campaign scripts pass the
#' root of the repository, so that they cannot write among the archived outputs. This follows
#' finding F2 of the review of the code of campaigns 2 and 3 of M1 (2026-11-07). Paths are
#' compared in their absolute form, without regard to case on Windows. The folder is not
#' created.
#'
#' @param dir output folder.
#' @param overwrite `TRUE` to allow an existing folder that is not empty, whose files the
#'   campaign then replaces (option `--overwrite` of the campaign scripts).
#' @param forbidden_root `NULL`, or a folder inside which `dir` must not lie.
#' @return The absolute path of `dir`, invisibly.
#' @export
check_output_folder <- function(dir, overwrite = FALSE, forbidden_root = NULL) {
  if (length(dir) != 1L || is.na(dir) || !nzchar(trimws(dir))) stop("give one output folder")
  abs_dir <- absolute_path(dir)
  if (!is.null(forbidden_root)) {
    root <- absolute_path(forbidden_root)
    norm <- function(x) if (.Platform$OS.type == "windows") tolower(x) else x
    root_slash <- if (endsWith(root, "/")) root else paste0(root, "/")
    if (norm(abs_dir) == norm(root) || startsWith(norm(abs_dir), norm(root_slash))) {
      stop(sprintf("the output folder %s lies inside %s, where no smoke check may write",
                   abs_dir, root))
    }
  }
  if (file.exists(dir) && !dir.exists(dir)) {
    stop(sprintf("the output folder %s is a file", abs_dir))
  }
  if (dir.exists(dir) && !isTRUE(overwrite) &&
      length(list.files(dir, all.files = TRUE, no.. = TRUE)) > 0L) {
    stop(sprintf(paste("the output folder %s exists and is not empty; nothing is written",
                       "(overwrite = TRUE, option --overwrite of the campaign scripts, allows it)"),
                 abs_dir))
  }
  invisible(abs_dir)
}

#' Write the run log of a campaign
#'
#' Writes `campaign_log.json` and, when `runs` is given, `runs.csv` in `dir`
#' (specification, section 10.4). Since walkbench 0.6.2, the log also records the kinds of
#' the random number generator (`rng_kind`, from [RNGkind()]) and the versions of pkgload and
#' parallel. It records the status of the whole repository that holds `campaign_path`
#' (`whole_repository`), besides that of `campaign_path` and of `package_path`.
#'
#' @param dir output folder of the campaign.
#' @param campaign,purpose,script,producer name and purpose of the campaign, script and
#'   producer of the script.
#' @param master_seed master seed, or a description of the seed rule.
#' @param design list that describes the design.
#' @param model the `simaci_model`.
#' @param injection injection point.
#' @param settings estimation settings.
#' @param start,end start and end times (POSIXct).
#' @param runs data frame of the summary rows, or `NULL`.
#' @param package_path folder of the package when it is loaded with `pkgload::load_all()`.
#' @param campaign_path folder of the repository that holds the campaign script.
#' @param extra list of further fields.
#' @return The path of `campaign_log.json`, invisibly.
#' @export
write_campaign_log <- function(dir, campaign, purpose, script, producer, master_seed,
                               design, model, injection, settings, start, end, runs = NULL,
                               package_path = NULL, campaign_path = ".", extra = list()) {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  pkg_desc <- utils::packageDescription("walkbench")
  pkg_git <- if (!is.null(package_path)) git_state(package_path) else
    list(commit = pkg_desc$RemoteSha %||% NA_character_, modified = NA_character_)
  camp_git <- git_state(campaign_path)
  repo_git <- git_state(campaign_path, whole = TRUE)
  vers <- function(p) tryCatch(as.character(utils::packageVersion(p)),
                               error = function(e) NA_character_)
  pin <- check_pinned_files()
  log <- list(
    campaign = campaign, purpose = purpose, script = script, producer = producer,
    master_seed = master_seed, design = design,
    model = list(id = model$id, label = model$label, adapter_version = model$version,
                 params = model$params),
    injection = injection, estimation_settings = settings,
    package = list(name = "walkbench", version = as.character(utils::packageVersion("walkbench")),
                   commit = pkg_git$commit, modified_files = pkg_git$modified,
                   loaded_from = if (is.null(package_path)) "installed" else
                     normalizePath(package_path, winslash = "/")),
    campaign_repository = list(commit = camp_git$commit, modified_files = camp_git$modified),
    whole_repository = list(commit = repo_git$commit, modified_files = repo_git$modified),
    pinned_files_identical = all(pin$identical),
    r_version = R.version.string, platform = R.version$platform,
    rng_kind = RNGkind(),
    package_versions = list(deSolve = vers("deSolve"), RANN = vers("RANN"), MASS = vers("MASS"),
                            digest = vers("digest"), jsonlite = vers("jsonlite"),
                            pkgload = vers("pkgload"), parallel = vers("parallel")),
    host = unname(Sys.info()[["nodename"]]),
    start = format(start, "%Y-%m-%dT%H:%M:%S%z"), end = format(end, "%Y-%m-%dT%H:%M:%S%z"),
    wall_time_s = as.numeric(difftime(end, start, units = "secs")))
  log <- c(log, extra)
  path <- file.path(dir, "campaign_log.json")
  jsonlite::write_json(log, path, auto_unbox = TRUE, pretty = TRUE, digits = NA,
                       na = "null", null = "null")
  if (!is.null(runs)) utils::write.csv(runs, file.path(dir, "runs.csv"), row.names = FALSE)
  invisible(path)
}
