

drop_extra_covariates = function( M0, data  ) {
  
  cfs = stats::coef( M0 )
  nas = names( cfs )[ is.na( cfs ) ]
  
  if ( length( nas ) > 0 ) {
    
    nas = paste0( nas, collapse = " - " )
    
    warning( paste0( "Dropped covariates due to colinearity with update of: ~ . - ", nas ) )
    
    stats::update( M0, formula. = stats::as.formula( paste( "~ . ", nas, sep= "-"  ) ), data=data )
  } else {
    # No need to take action
    M0
  }
}




#' Generate an ITS extrapolation simulation.
#'
#' Take a given outcome variable, fit an ITS model, use it to extrapolate R
#' plusible trajectories, and then using these trajectories, generate final
#' impact results by averaging (if summarize is set to TRUE).
#'
#' @param outcomename  Name of column in dat containing the time series.
#' @param dat Dataframe with a 'month' column for time.
#' @param t0 Last pre-policy timepoint
#' @param R Number of simulated pre-policy extrapolations to generate.
#' @param summarize Summarise the series? (TRUE/FALSE)
#' @param smooth Smooth the series? (TRUE/FALSE)
#' @param smoother Function to smooth residuals, if smoothing set to TRUE.  If
#'   NULL, will dynamically make a model smoother based on the fit_model method.
#' @param fit_model The function used to fit the model to simulate from. (This
#'   model could be a seasonality model. Default is simple linear model with no
#'   covariates.)
#' @param covariates Vector of covariate names of all covariates used in the
#'   passed model function fit_model.  If null, will attempt to get list of
#'   covariates form the "lags" attribute of the passed 'fit_model'.
#' @param plug.in Use the estimated parameters as fixed and do not include extra
#'   uncertainty of parameter estimation in the simulation. (Not recommended as
#'   it destroys inference.)
#' @param ... Extra arguments to be passed to other function.
#' @return If summarize=TRUE, A dataframe with several columns of interest and
#'   one row per month of data. The columns are Ymin and Ymax, the limits of the
#'   envelope, 'range', the range of the envelope, 'SE', the standard deviation
#'   of the trajectories at that time point, `Ysmooth` the median smoothed value
#'   at that time point (if smoothing), `Ystar` the median unsmoothed value at
#'   that time point (regardless of smooth flag), `Y`, the observed outcome,
#'   `Ysmooth1`, the smoothed observed outcomes, and `Ybar` the predicted
#'   outcome given the model with no autoregressive aspect.
#'
#'   If summarize=FALSE, a dataframe of all the raw series generated.
#' @export
process_outcome_model = function( outcomename, dat, t0, R=400, summarize=FALSE,
                                  smooth=FALSE, smoother = NULL,
                                  fit_model = fit_model_default,
                                  covariates = NULL,
                                  plug.in = FALSE, ... ) {
  
  if ( is.null( covariates ) ) {
    covariates = attr( fit_model, "lags" )
  }
  
  dat = add_lagged_covariates( dat, outcomename, covariates = covariates  )
  
  dat.pre = dplyr::filter( dat, month <= t0 )
  
  M0 = fit_model( dat.pre, outcomename )
  
  if ( any( is.na( stats::coef( M0 ) ) ) ) {
    M0 = drop_extra_covariates( M0, dat.pre[-c(1),] )
  }
  
  # If smoothing, generate the smoother function to pass to extrapolate_model
  if ( smooth && is.null( smoother ) ) {
    M0full = stats::model.frame( M0, data=dat, na.action=NULL )
    smoother = make_model_smoother( covariates=M0full, fit_model = fit_model )
  }
  
  res = extrapolate_model( M0, outcomename, dat, t0, R, summarize=summarize, 
                           smooth=smooth, smoother=smoother, 
                           fix.params = plug.in )
  
  if ( summarize ) {
    res$Ybar = generate_Ybars( fit_model, outcomename, t0, dat )
  }
  
  res
}

