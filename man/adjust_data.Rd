% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/post_stratified_ITS.R
\name{adjust_data}
\alias{adjust_data}
\title{Adjust an outcome time series based on the group weights.}
\usage{
adjust_data(
  dat,
  outcomename,
  groupname,
  pi_star,
  is_count = FALSE,
  include_aggregate = FALSE,
  covariates = NULL
)
}
\arguments{
\item{dat}{Dataframe of data.  Requires an N column of total cases
represented in each row.}

\item{outcomename}{Name of column that has the outcome to calculated adjusted
values for.}

\item{groupname}{Name of categorical covariate that determines the groups.}

\item{pi_star}{The target weights.  Each month will have its groups
re-weighted to match these target weights.}

\item{is_count}{Indicator of whether outcome is count data or a continuous
measure (this impacts how aggregation is done).}

\item{include_aggregate}{Include aggregated (unadjusted) totals in the output
as well.}

\item{covariates}{Covariates to be passed to aggregation (list of string
variable names).}
}
\description{
Reweight the components of a series to match target weights for several
categories. This is a good preprocessing step to adjust for time-varying
covariates such as changing mix of case types.
}
