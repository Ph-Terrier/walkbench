# Title: R code exported by the web simulator for the reference run m1_arfima
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
m <- model_m1()
inp <- input_arfima(620, d = 0.3, sd = 0.00234, seed = 7, n_transient = 20)
run <- trim_run(simulate_run(m, inp, n_cycles = 620), n_transient = 20)
s <- m$estimation_settings
s$window_s <- c(0, 0.1)
s$window_l <- c(3, 4)
s$horizon <- 4
row <- estimate_all(run, m, settings = s, input = inp)
row[, c("lambda_s", "lambda_l", "lambda_s_per_step", "lambda_l_per_step",
        "alpha_input", "alpha_duration", "alpha_stride_duration")]
