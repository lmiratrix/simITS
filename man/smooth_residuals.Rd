% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ITSModelingCode.R
\name{smooth_residuals}
\alias{smooth_residuals}
\title{Smooth residuals after model fit}
\usage{
smooth_residuals(
  res,
  t0 = 0,
  outcomename = "Y",
  timename = "time",
  post.only = TRUE,
  smooth_k = SMOOTH_K,
  fit_model = fit_model_default,
  covariates = res,
  full_output = FALSE
)
}
\arguments{
\item{res}{A dataframe with a 'timename' column and an 'outcomename' column (which
is the column that will be smoothed).}

\item{t0}{last pre-policy timepoint}

\item{outcomename}{String name of the outcome variable in dat.  Defaut is "Y".}

\item{timename}{Name of the time column.  Default is "time".}

\item{post.only}{If TRUE fit model and smooth post-policy only. WHY fit model
on post-policy data only?  Because this will make sure the fixed pre-policy
does not dominate too much?  We are focusing on post-policy so we want a
good fitting model for that so we can get our residuals as "white noise" as
possible before smoothing.}

\item{smooth_k}{A rough proxy for the number of observations to primarily
consider to kernal weight in the neighborhood of each timepoint (this is a
bandwidth, and the loess smoother gets smooth_k / n as a span value).  We
want to smooth with an absolute bandwidth, not one as function of how long
the time series is.}

\item{fit_model}{A function that takes data, fits a linear model, and returns
the fit model. This function needs an option to include (or not) lagged
covariates.}

\item{covariates}{A dataframe with all covariates needed in the model fitting
defined by fit_model.}

\item{full_output}{If TRUE give back pieces for diagnostics of smoothing
process.}
}
\value{
A numeric vector of the smoothed residuals.  If full_output=TRUE
  return a dataframe with several other columns: `resid`, the residuals based
  on Ystar and the model, `residStar` the smoothed residuals, 'Ybar.sm' the
  structural predictions of the model used for smoothing.  Here the smoothed
  values will be 'Ysmooth'.
}
\description{
Smooth a series by fitting the model to the data, smoothing the residuals,
and then putting the model predictions back.
}
\details{
Use loess smoother on complete series of residuals including original data
pre-policy and synthetic data post policy (i.e., smooth the entire plausible
series).
}
\examples{

data( "newjersey" )
smooth = smooth_series( newjersey, outcomename = "n.warrant", t0= -8,
                        smooth_k = 30,
                        post.only = FALSE)
plot( newjersey$month, newjersey$n.warrant )
lines( newjersey$month, smooth, col="red" )  

mod =  make_fit_season_model( ~ temperature )
newjersey = add_lagged_covariates( newjersey, outcomename = "n.warrant", 
                                   covariates = c("temperature") )

smooth = smooth_residuals( newjersey, outcomename = "n.warrant", t0=-8,
                           smooth_k = 30,
                           post.only = FALSE,
                           fit_model = mod )
plot( newjersey$month, newjersey$n.warrant )
lines( newjersey$month, smooth, col="red" )  
}
\seealso{
See \code{\link{smooth_series}} for a more vanilla version that
  smooths without the model fitting step.
}
