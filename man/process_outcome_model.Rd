% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/process_outcome_model.R
\name{process_outcome_model}
\alias{process_outcome_model}
\title{Generate an ITS extrapolation simulation.}
\usage{
process_outcome_model(
  dat,
  outcomename = "Y",
  timename = "time",
  t0 = 0,
  R = 400,
  summarize = FALSE,
  smooth = FALSE,
  smoother = NULL,
  fit_model = fit_model_default,
  covariates = NULL,
  plug_in = FALSE,
  ...
)
}
\arguments{
\item{dat}{Dataframe with a column for time.  The time column is
assumed to be a sequence of integer values.}

\item{outcomename}{Name of column in dat containing the observed
outcomes.}

\item{timename}{Name of column in dat containing the time points.}

\item{t0}{Last pre-policy timepoint}

\item{R}{Number of simulated pre-policy extrapolations to generate.}

\item{summarize}{Summarise the series? (TRUE/FALSE)}

\item{smooth}{Smooth the series? (TRUE/FALSE)}

\item{smoother}{Function to smooth residuals, if smoothing set to
TRUE.  If NULL, will dynamically make a model smoother based on
the fit_model method if covariates are passed.  Otherwise it will
use the simple smoother on the outcomes.}

\item{fit_model}{The function used to fit the model to simulate
from. (This model could be a seasonality model. Default is simple
linear model with no covariates.)}

\item{covariates}{Vector of covariate names of all covariates used
in the passed model function fit_model.  If null, will attempt to
get list of covariates form the "lags" attribute of the passed
'fit_model'.}

\item{plug_in}{Use the estimated parameters as fixed and do not
include extra uncertainty of parameter estimation in the
simulation. (Not recommended as it destroys inference.)}

\item{...}{Extra arguments to be passed to `extrapolate_model()`}
}
\value{
If summarize=TRUE, A dataframe with several columns of
  interest and one row per time point of data. The columns are Ymin and
  Ymax, the limits of the envelope, 'range', the range of the
  envelope, 'SE', the standard deviation of the trajectories at
  that time point, `Ysmooth` the median smoothed value at that time
  point (if smoothing), `Ystar` the median unsmoothed value at that
  time point (regardless of smooth flag), `Y`, the observed
  outcome, `Ysmooth1`, the smoothed observed outcomes, and `Ybar`
  the predicted outcome given the model with no autoregressive
  aspect.

  If summarize=FALSE, a dataframe of all the raw series generated.
}
\description{
This is the primary function to use to use this approach on a given
dataset.
}
\details{
Take a given outcome variable, fit an ITS model, use it to
extrapolate R plusible trajectories, and then using these
trajectories, generate final impact results by averaging (if
summarize is set to TRUE).

This function is basically a wrapper for `extrapolate_model()` with
some extra calls to `make_model_smoother()` to prepare, in the case
of smoothing, and adding on a summary trend via `generate_Ybars()`
in the case of summarizing.
}
\examples{
data( "mecklenberg" )
t0 = 0
envelope = process_outcome_model( "pbail", mecklenberg, 
                                  t0=t0, R = 10, 
                                  summarize = TRUE, smooth=FALSE )
make_envelope_graph(envelope, t0=t0, ylab = "Proportion given bail") +
  ggplot2::labs( title="Sample ITS plot")
}
\seealso{
The core internal function that this method is a wrapper
  for is \code{\link{extrapolate_model}}.
}
