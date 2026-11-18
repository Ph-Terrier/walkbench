# Title: Entry point of the package tests (testthat)
# Author: Philippe Terrier (HE-Arc Sante, Neuchatel), responsible author
# Produced by: written by the SP1.1 session of SIM-ACI (Claude Opus 5.5), without a
#   code-generation model
# Date: 2026-11-05
# Description: runs the tests of tests/testthat/ under R CMD check
# Dependencies: testthat, walkbench
# Input: none
# Output: none

library(testthat)
library(walkbench)

test_check("walkbench")
