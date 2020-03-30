#' \code{simITSA} package
#'
#' Analysis via Simulation of Interrupted Time Series
#'
#'
#' @docType package
#' @name simITS
#' @importFrom dplyr %>%
NULL

## quiets concerns of R CMD check re: the .'s that appear in pipelines
globalVariables(names = c(".Y.adj", "M", "N", "Q1", "Q2", "Q3", "Ybar", "Ymax", "Ymin", "Ysmooth", "Ystar", "month"), package = "simITS", add = FALSE)
