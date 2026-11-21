# Title: Package documentation and package-level state of walkbench
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05 (version of the help page updated on 2026-11-07 for walkbench 0.6.1 and 0.6.2,
#   and on 2026-11-20 for walkbench 0.7.0 by the same session: version, predictor named without
#   the sub-project; man/ regenerated with roxygen2 8.0.0)
# Description: package help page and the private environment that caches the pinned code
#   loaded at run time (walker functions of M1, RACING engine). Specification:
#   SIMACI/wp1_replication/interface_spec.md, version 0.2.
# Dependencies: RANN (imported here because the pinned RACING engine calls RANN::nn2())
# Input: none
# Output: none

#' walkbench: testbed of walking models for the divergence exponents of gait
#'
#' The package exposes walking models through a common interface (specification 0.2 of
#' the SIM-ACI project, sub-project SP1.1): a perturbation port ([simaci_input()]), a
#' closed-loop port ([make_controller_h3a()], [make_controller_h3b()],
#' [make_metronome()]), a linearization at the fixed point ([linearize()]), standardized
#' outputs ([simulate_run()], [trim_run()]), common estimators ([estimate_all()]) and a run
#' driver with seeds and run log ([run_trial()], [write_campaign_log()]), and the campaign rules of
#' the analysis plan frozen at commit ba72afe ([campaign_table()], [summarize_cells()]), and the analytic
#' predictor ([predictor_levels()]) with the extension to M2 of update 1 of the
#' registration osf.io/m74tk ([predictor_settings_m2()]). The functions of the predictor keep
#' their former names, such as [predict_spinal_levels()] and [spinal_settings_m2()]. Version 0.7.0
#' contains the models M1 ([model_m1()]) and M2 ([model_m2()]).
#'
#' @keywords internal
#' @importFrom RANN nn2
"_PACKAGE"

# Private cache of the pinned code loaded at run time
.wb <- new.env(parent = emptyenv())

`%||%` <- function(a, b) if (is.null(a)) b else a
