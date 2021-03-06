% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simITS.R
\docType{package}
\name{simITS}
\alias{simITS}
\title{\code{simITS} package overview}
\description{
Analysis via Simulation of Interrupted Time Series
}
\details{
This package is based on the backbone analytic code for the analyses in,
e.g., "Evaluation of Pretrial Justice System Reforms That Use the Public
Safety Assessment: Effects in Mecklenburg County, North Carolina" (see MDRC
policy brief at <https://www.mdrc.org/publication/evaluation-pretrial-justice-system-reforms-use-public-safety-assessment)>.

This package provides methods for fitting Interrupted Time Series models with
laged outcomes and variables to account for temporal dependencies.  It then
conducts inference via simulation, simulating a set of plausble
counterfactual post-policy series to compare to the observed post-policy
series. This package provides methods to visualize such data, and also to
incorporate seasonality models and smoothing and aggregation/summarization.
See companion paper "Using Simulation to Analyze Interrupted Time Series
Designs" at <https://arxiv.org/abs/2002.05746> for discussion of the
overall approach.
}
