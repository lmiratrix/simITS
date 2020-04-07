#' \code{simITS} package overview
#'
#' Analysis via Simulation of Interrupted Time Series
#'
#' This package is based on the backbone analytic code for the analyses in,
#' e.g., "Evaluation of Pretrial Justice System Reforms That Use the Public
#' Safety Assessment: Effects in Mecklenburg County, North Carolina" (see MDRC
#' policy brief at <https://www.mdrc.org/publication/evaluation-pretrial-justice-system-reforms-use-public-safety-assessment)>.
#'
#' This package provides methods for fitting Interrupted Time Series models with
#' laged outcomes and variables to account for temporal dependencies.  It then
#' conducts inference via simulation, simulating a set of plausble
#' counterfactual post-policy series to compare to the observed post-policy
#' series. This package provides methods to visualize such data, and also to
#' incorporate seasonality models and smoothing and aggregation/summarization.
#' See companion paper "Using Simulation to Analyze Interrupted Time Series
#' Designs" at <https://arxiv.org/abs/2002.05746> for discussion of the
#' overall approach.
#'
#' @docType package
#' @name simITS
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
globalVariables(names = c(".Y.adj", "M", "N", "Q1", "Q2", "Q3", "Ybar", "Ymax", "Ymin", "Ysmooth", "Ystar", "month"), package = "simITS", add = FALSE)
