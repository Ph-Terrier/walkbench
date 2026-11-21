# Title: R code exported by the web simulator for the reference run m2_arfima
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: exported by the page of testbed/web (js/wb_app.js, function rCode) on
#   2026-11-20, in the browser pane of the Claude desktop app; this header and the comment that
#   replaces library(walkbench) added by the orchestrating model of the web-simulator session of
#   SIM-ACI (Claude Opus 5.5); first line changed from "0.6.2 or later" to "0.7.0 or later" on
#   2026-11-20, as the page now writes it; the installation comment that follows it added on
#   2026-11-20 for the same reason
# Rerun in walkbench (version 0.7.0 or later) of the reference run of the web simulator
# install.packages("remotes"); remotes::install_github("Ph-Terrier/walkbench")
# library(walkbench): replaced by load_all() in check_exports.R (walkbench is not installed on the laptop)
m <- model_m2(gamma = 0.004, r_f = 0.45, k = 5,
              x_init = c(0.17184140584493454, -0.27465602777778253, 0.3436828116898686, -0.008800993038983606))
inp <- input_arfima(634, d = 0, sd = 0.00018, seed = 1, n_transient = 34)
run <- trim_run(simulate_run(m, inp, n_cycles = 634), n_transient = 34)
s <- m$estimation_settings
s$window_s <- c(0, 0.5)
s$window_l <- c(4, 10)
s$horizon <- 10
row <- estimate_all(run, m, settings = s, input = inp)
row[, c("lambda_s", "lambda_l", "lambda_s_per_step", "lambda_l_per_step",
        "alpha_input", "alpha_duration", "alpha_stride_duration")]
