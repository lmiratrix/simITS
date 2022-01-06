#' \code{simITS} package overview
#'
#' Analysis via Simulation of Interrupted Time Series
#'
#' This package is based on the backbone analytic code for the analyses in,
#' e.g., Redcross et al. (2019) or Golub et al. (2019).  See companion paper
#' Miratrix (2020) for technical discussion of the overall approach.
#'
#' Broadly, this package provides methods for fitting Interrupted Time Series
#' models with lagged outcomes and variables to account for temporal
#' dependencies.  It then conducts inference via simulation, simulating a set of
#' plausible counterfactual post-policy series to compare to the observed
#' post-policy series. This package provides methods to visualize such data, and
#' also to incorporate seasonality models and smoothing and
#' aggregation/summarization. See the vignette for a guide of how to conduct
#' such analyses.
#'
#' @references Redcross, C., Henderson, B., Valentine, E. & Miratrix, L. (2019).
#'   Evaluation of pretrial justice system reforms that use the public safety
#'   assessment: Effects in Mecklenburg County, North Carolina. Technical
#'   report, MDRC \href{https://www.mdrc.org/publication/evaluation-pretrial-justice-system-reforms-use-public-safety-assessment}{(link)}
#'
#'   Golub, C. A., Redcross, C., Valentine, E., & Miratrix, L. (2019).
#'   Evaluation of pretrial justice system reforms that use the public safety
#'   assessment: Effects of New Jersey’s criminal justice reform. Technical
#'   report, MDRC. \href{https://www.mdrc.org/publication/evaluation-pretrial-justice-system-reforms-use-public-safety-assessment-0}{(link)}
#'
#'   Miratrix, L. (2020). Using Simulation to Analyze Interrupted Time Series
#'   Designs \href{https://arxiv.org/abs/2002.05746}{(link)}
#'
#' @docType package
#' @name simITS
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
globalVariables(names = c(".Y.adj", "M", "N", "Q1", "Q2", "Q3", "Ybar", "Ymax", "Ymin", "Ysmooth", "Ystar", "month"), package = "simITS", add = FALSE)
