
# Default smoothing value
SMOOTH_K = 11




#' Generate the model predictions for a series, ignoring lagged components.
#'
#' Generate Ybar given a dataframe 'dat'. Ybar is our model predictions without
#' any lagged terms, fit to the pre-policy data only. This is basically the
#' "structural" component of our model, which can include seasonality.
#'
#' This is so we can show trendlines and so forth on plots.
#'
#' Ybar is defined for both pre and post policy datapoints.
#' @param fit_model A function that takes data, fits a linear model, and returns
#'   the fit model. This function needs an option to include (or not) lagged
#'   covariates.
#' @param outcomename String name of the outcome variable in dat.
#' @param t0 last pre-policy timepoint
#' @param dat Dataframe of the data.
#' @return Predicted sequence for passed dataframe (as vector)
#' @noRd
generate_Ybars = function( fit_model, outcomename, t0, dat ) {

  # refit model with no_lags
  #fm = formula( fit0 )
  #vrs = all.vars( fm )
  #lgs = grep( "lag.", vrs, value=TRUE )
  #fmup = paste0( "~ . - ", paste( lgs, collapse =" - " ), collapse="" )

  #fit0.lagless = stats::update( fit0, fmup, data=dat )
  M0.lagless = fit_model( dplyr::filter( dat, month <= t0 ), outcomename, lagless=TRUE )

  stats::predict( M0.lagless, newdata = dat )
}




#' Make a synthetic timeseries with autoregressive structure
#' 
#' Given model parameters, make a synthetic timeseries for all points after t0.
#' This series is a plausible continuation of a pre-policy series.  This method needs
#' to be given the structural component, calculated from the intercept, linear trend,
#' temperature, and seasonality.
#
#' Bundling all seasonality, etc., into Ybar, the assumed model is:
#'
#'    $$Y = Ybar + beta1 * Y.pre + residual$$
#'
#' where Ybar is our structural model (with lagged OUTCOME, not lagged residual, so the generate_Ybars
#' method will not give the correct values here).
#
#' @param Ybar Structural (expected) sequence.  This method adds the autoregressive residual component
#'       to this.  Ybar tends to come from generate_structure_sequence()
#' @param Y.init Value of series at time point just before first time point in dat.post
#' @param beta1 Coefficient for lagged outcome
#' @param sigma Standard deviation of the residual errors to add to each step
#' @param expected If TRUE, return the expected value of the series with no residual noise
#' @param include.start If TRUE, return Y.init as the initial element in the returned sequence.
#'
#' @return Vector of the synthetic outcomes.
#' @noRd
make_autoregressive_series = function( Ybar, Y.init, beta1, sigma,
                                       expected=FALSE, include.start = FALSE ) {
    if ( expected ) {
      Ys = c( Y.init, rep( 0, length(Ybar) ) )
    } else {
      Ys = c( Y.init, stats::rnorm( length( Ybar ), mean=0, sd=sigma ) )
    }
    for ( i in seq_along( Ybar ) ) {
        Ys[i+1] = Ybar[[i]] + beta1*Ys[[i]] + Ys[[i+1]]
    }

    if ( include.start ) {
      Ys
    } else {
      Ys[-1]
    }
}


#' Generate the predictions of fit0 excluding the lagged outcome variable.
#'
#' This is a utility function used by the make_autoregressive_series() method.
#'
#' We do this so we can subtract this part off before smoothing so our smoother
#' doesn't have to try and capture the seasonality stuff.
#' @param fit0 Model fit to data.
#' @param dat Data to predict outcomes for using fit0.
#' @return Predictions as a numeric vector.
#' @noRd
generate_structure_sequence = function( fit0, dat ) {
  dat$lag.outcome = 0
  stats::predict( fit0, newdata=dat )
}



#' Make a simulated series of time points that follow a fit model.
#' 
#' Given a model captured in beta.vec (the vector of coefficients from a
#' regression held in 'fit0') and sigma (the standard deviation of the
#' residuals), make a simulated series of time points that follow the model.
#'
#' @param beta.vec Vector of parameter values
#' @param sigma Value for standard deviation of independent portion of residual
#' @param dat The dataframe (pre and post policy)
#' @param fit0 The original model fit to the data
#' @param outcomename The outcome of interest
#' @param t0 The last pre-policy timepoint.
#'
#' @return Dataframe, one row per timepoint.  Columns are 'month' (the time),
#'   'Ybar' the predicted outcomes from the model, and 'Ystar', a concatenation
#'   of the pre-policy series followed by the synthetic sequence.
#' @noRd
generate_prediction_sequence = function( beta.vec, sigma,
                                         dat, fit0, outcomename, t0 ) {
    fit0$coefficients = beta.vec
    beta1 = stats::coef( fit0 )[[ "lag.outcome" ]]

    # Predict what we would see, setting lagged outcome to 0 (to set up simulation).
    dat$Ybar.core = generate_structure_sequence( fit0, dat=dat )

    # Calculate what the parameters would suggest we would see in expectation, conditional on the first
    # observed timepoint.
    dat$Ybar = make_autoregressive_series( dat$Ybar.core[-c(1)], dat[[outcomename]][1], beta1=beta1, sigma=sigma,
                                           expected = TRUE,
                                           include.start = TRUE)

    dat.post = dplyr::filter( dat, month > t0 )
    dat.pre = dplyr::filter( dat, month <= t0 )
    dat.thresh = dat.pre[ nrow( dat.pre ), ]

    dat.post$Ystar = make_autoregressive_series( dat.post$Ybar.core,
                                                 Y.init=dat.thresh[[outcomename]], beta1=beta1, sigma=sigma )

    # For prepolicy data, keep our structural predictions, but
    # our observed data are our 'Ystar'
    prepolicy = data.frame( month = dat.pre$month,
                            Ybar = dat.pre$Ybar,
                            Ystar = dat.pre[[outcomename]] )

    dplyr::bind_rows( prepolicy,
               dplyr::select( dat.post, month, Ybar, Ystar ) )
}


#' Smooth a series using a static loess smoother
#'
#' Use loess smoother on complete series of residuals including original data
#' pre-policy and synthetic data post policy (i.e., smooth the entire plausible
#' series).
#'
#' This method takes several parameters it does not use, to maintain
#' compatability with smooth_residuals.
#'
#' @param res A dataframe with a month column and an 'outcomename' column (which
#'   is the column that will be smoothed).
#' @param smooth_k A rough proxy for the number of observations to primarily
#'   consider to kernal weight in the neighborhood of each timepoint (this is a
#'   bandwidth, and the loess smoother gets smooth_k / n as a span value).  We
#'   want to smooth with an absolute bandwidth, not one as function of how long
#'   the time series is.
#' @param outcomename String name of the outcome variable in dat.
#' @param t0 last pre-policy timepoint
#' @param ... Extra arguments (not used in this function).
#' @param post.only If TRUE fit model and smooth post-policy only. WHY fit model
#'   on post-policy data only?  Because this will make sure the fixed pre-policy
#'   does not dominate too much?  We are focusing on post-policy so we want a
#'   good fitting model for that so we can get our residuals as "white noise" as
#'   possible before smoothing.
#'
#' @return An updated version of the 'res' dataframe with `Ysmooth`, the
#'   smoothed predictions of the original Ystar outcome.  Also includes 'Ystar'
#'   the original sequence to be smoothed.
#'
#' @example examples/example_smooth_residuals.R
#' 
#' @export
#' @importFrom rlang .data
#' @importFrom dplyr mutate
#' @importFrom stats formula
smooth_series = function( res, outcomename, t0,
                          smooth_k = SMOOTH_K,
                          post.only = TRUE,
                          ... ) {

  if ( post.only ) {
    res.dat = dplyr::filter( res, .data$month > t0 )
  } else {
    res.dat = res
  }

  form = paste0( outcomename, " ~ month" )
  mod = stats::loess( formula( form ), data=res.dat, span=smooth_k / nrow(res.dat) )

  res = mutate( res,
                Ysmooth = stats::predict( mod, newdata=res ) )

  # Unneeded, loess won't extrapolate
  #if ( post.only ) {
  #  res$Ysmooth[ res$month <= t0 ] = NA
  #}

  res$Ysmooth
}





#' Smooth residuals after model fit
#'
#' Smooth a series by fitting the model to the data, smoothing the residuals,
#' and then putting the model predictions back.
#'
#' Use loess smoother on complete series of residuals including original data
#' pre-policy and synthetic data post policy (i.e., smooth the entire plausible
#' series).
#'
#' @inheritParams smooth_series
#' @param fit_model A function that takes data, fits a linear model, and returns
#'   the fit model. This function needs an option to include (or not) lagged
#'   covariates.
#' @param full_output If TRUE give back pieces for diagnostics of smoothing
#'   process.
#' @param covariates A dataframe with all covariates needed in the model fitting
#'   defined by fit_model.
#'
#' @return A numeric vector of the smoothed residuals.  If full_output=TRUE
#'   return a dataframe with several other columns: `resid`, the residuals based
#'   on Ystar and the model, `residStar` the smoothed residuals, 'Ybar.sm' the
#'   structural predictions of the model used for smoothing.  Here the smoothed
#'   values will be 'Ysmooth'.
#' @example examples/example_smooth_residuals.R
#'   
#' @export
#' @seealso See \code{\link{smooth_series}} for a more vanilla version that
#'   smooths without the model fitting step.
#' @importFrom rlang .data
#' @importFrom dplyr mutate filter
smooth_residuals = function( res, t0, outcomename,
                             post.only = TRUE,
                             smooth_k = SMOOTH_K,
                             fit_model = fit_model_default,
                             covariates = res,
                             full_output = FALSE ) {
    stopifnot( nrow( res ) == nrow( covariates ) )

    covariates$Y = res[[outcomename]]

    if ( post.only ) {
      res.dat = dplyr::filter( covariates, .data$month > t0 )
    } else {
      res.dat = covariates
    }

    fit0star = fit_model( res.dat, "Y", lagless=TRUE )

    covariates$Ybar.sm = stats::predict( fit0star, newdata=covariates )

    covariates = mutate( covariates, resid = .data$Y - .data$Ybar.sm )

    covariates$residStar = smooth_series( covariates, "resid", t0, 
                                          smooth_k=smooth_k, post.only=post.only )

    covariates = mutate( covariates,
                         Ysmooth = .data$Ybar.sm + .data$residStar )

    # Give back columns of interest (for debugging)
    if ( full_output ) {
      dplyr::select( covariates, .data$month, .data$resid, .data$residStar, .data$Ybar.sm, .data$Y, .data$Ysmooth )
    } else {
      covariates$Ysmooth
    }
}



#' Make a smoother that fits a model and then smooths residuals
#'
#' This helper function gives back a function that takes the resulting
#' simulation data from a single iteration of the simulation, and fits
#' 'fit_model' to it, smoothes the residuals, and puts the predictions from
#' 'fit_model' back.
#'
#' This can be used to build smoothers that smooth using models other than the
#' model being used for extrapolation (e.g., a model without temperature).
#'
#' Resulting functions have the following parameters: `res` (the data), `t0`
#' (start time), `outcomename`, `post.only` flag (for smoothing only post data
#' or not), and `smooth_k`, a tuning parameter for degree of smoothing.
#'
#'
#' @inheritParams smooth_residuals
#'
#' @return A smoother function that can be passed to the smoothing routines.
#'   This function is of the form listed above.
#' @example examples/make_model_smoother.R
#'
#' @export
make_model_smoother = function( fit_model, covariates ) {

  f = function( res, t0, outcomename, post.only = TRUE, smooth_k = SMOOTH_K ) {
    smooth_residuals( res=res, t0=t0, 
                      outcomename=outcomename, 
                      post.only=post.only, 
                      smooth_k=smooth_k,
                      fit_model=fit_model, 
                      covariates=covariates )
  }

  return( f )
}







#' Generate a collection of raw counterfactual trajectories
#'
#' Given a fit linear model 'fit0', generate R prediction series starting at t0.
#' This takes model uncertainty into account by pulling from the
#' pseudo-posterior of the model parameters (from Gelman and Hill arm package).
#'
#' @param fit0 The fit linear model to simulate from.
#' @param R Number of series to generate.
#' @param t0 Last month of pre-policy.  Will start predicting at t0+1.
#' @param dat A dataframe with the covariates needed by the model fit0 for both
#'   pre and post-policy months.
#' @param outcomename The name of the column in dat which is our outcome.
#' @references The `arm` package, see
#'   \url{https://cran.r-project.org/package=arm}
#'
#'   Also see Gelman, A., & Hill, J. (2007). Data analysis using regression and
#'   multilevelhierarchical models (Vol. 1). New York, NY, USA: Cambridge
#'   University Press.
#'
#' @return A data.frame with the collection of predicted series, one row per
#'   month per replicate (so will have R*nrow(dat) rows).
#'
#' @example examples/make_many_predictions.R
#'
#' @export
make_many_predictions = function( fit0, dat, R, outcomename, t0 ) {

    # Generate collection of plausible beta values and sigma values we
    # will use for our predictions
    a = arm::sim( fit0, n.sims=R )
    coefs = stats::coef( a )

    # How often are our posterior lag.outcome coefficients larger than 1, which
    # creates expotential blow-up in our series.
    if( mean( coefs[,"lag.outcome"] > 1.0 ) > 0.01 ) {
        warning( sprintf( "Substantial estimated coefficients (%.1f percent) for lagged outcome are greater than 1 which will give substantial instability.",
                          100*mean( mean( coefs[,"lag.outcome"] > 1.0 )  ) ) )
    }
    coefs = as.data.frame( coefs )

    # get names of coefficients that were not dropped (e.g., due to colinearity)
    cc = names( stats::coef( fit0 )  )[ !is.na( stats::coef( fit0 ) ) ]

    colnames( coefs ) = cc

    # This dark arts code makes the coefficient matrix a list of named vectors.
    # So each element of the list is a vector of coefficients. Split divides coefs
    # into a list of 1-row dataframes, as.list converts the dataframe to a list
    # and unlist converts that back to a vector.  The map does it to each of our 1
    # row dataframes we get from the split() call.
    coefs = split( coefs, seq( nrow(coefs) ) ) %>%
        purrr::map( as.list ) %>% purrr::map( unlist )

    # get our sigmas
    sigmas = a@sigma
    #coefs$sigma = sigmas
    #names( coefs ) = c( "beta0", "beta1", "beta2", "beta3", "beta4", "beta5", "beta6", "sigma" )


    # For each plausible coefficient vector, generate a predictive series.
    res = purrr::map2_dfr( coefs, sigmas, generate_prediction_sequence, .id="Run",
                    dat=dat, fit0 = fit0, outcomename=outcomename, t0=t0 )

    res
}




#' @describeIn make_many_predictions This version makes multiple predictions
#'   using estimated parameters without additional uncertainty. This takes point
#'   estimates from the fit model as fixed parameters. WARNING: This method will
#'   not capture true uncertainty as it is not taking parameter uncertainty into
#'   account.
#' @export
make_many_predictions_plug = function( fit0, dat, R, outcomename, t0 ) {

  # Repeatidly generate a predictive series.
  res = plyr::rdply( R, generate_prediction_sequence( stats::coef( fit0 ), stats::sigma( fit0 ),
                                                      dat=dat, fit0 = fit0, outcomename=outcomename, t0=t0 ),
                     .id = "Run" )

  res
}



#' Augment dataframe with lagged covariates
#'
#' Take outcome and a list of covariates and add new columns with lagged
#' versions.  Assumes rows of dataframe are in time ascending order.  Lagged
#' outcome canonically called 'lag.outcome'.  Covariates 'lag.XXX'.
#' 
#' @param dat   The dataframe
#' @param outcomename   The outcome of interest (string)
#' @param covariates The covariates to lag along with the outcome. This can be
#'   either of two things.  First, it can be a list of string names.  Covariates
#'   can also be a function with a "lags" attribute with the listed covariates
#'   (as returned by, e.g., make_fit_season_model)  (which is a list of string
#'   names). NULL if no covariates other than outcome should be lagged.
#'
#' @return Augmented dataframe with lagged covariates as new columns. Will
#'   clobber old columns if the names (of form "lag.XXXX") conflict.
#' @examples
#' data( "newjersey" )
#' newjersey = add_lagged_covariates(newjersey, "n.warrant", c("sin.m","cos.m" ) )
#' head( newjersey[ c( "n.warrant", "sin.m", "lag.outcome", "lag.sin.m" ) ] )
#' @export
add_lagged_covariates = function( dat,
                                  outcomename,
                                  covariates = NULL ) {
  if ( !( outcomename %in% names(dat) ) ) {
    stop( sprintf( "Outcome '%s' is not a column in passed data", outcomename ) )
  }
  
  # make lagged covariates for modeling
  dat = dplyr::mutate( dat, lag.outcome = dplyr::lag( dat[[outcomename]] ) )

  if ( !is.null( covariates ) ) {
    if ( !is.null( attr( covariates, "lags" ) ) ) {
      covariates = attr( covariates, "lags" )
    }
  }
  
  if( !is.null( covariates ) ) {
    lc = paste0( "lag.", covariates )
    for ( i in seq_along( covariates ) ) {
      dat[ lc[[i]] ] = dplyr::lag( dat[[ covariates[i] ]] )
    }
  }

  dat
}






#' Extrapolate pre-policy data to post-policy era
#'
#' This function takes a fitted model and uses it to make the post-policy
#' predictions by simulating data.
#'
#' @param M0  The fit model
#' @param outcomename Outcome of interest (name of column)
#' @param dat Dataframe with data being analyzed.
#' @param t0  Last pre-policy timepoint
#' @param R  Number of replications
#' @param summarize Boolean, TRUE means collapse all simulated trajectories into
#'   single aggregate. FALSE means return all paths.
#' @param smooth Boolean.  TRUE means fit a smoother to the trajectories and
#'   look at distribution of smoothed trajectories.  FALSE means look at raw
#'   data treajectories.
#' @param smoother  Function to do smoothing, if smoothing set to TRUE.  Default
#'   is smooth_series()
#' @param fix_parameters Keep the parameters in the model M0 as fixed; do not
#'   add parameter uncertainty.
#' @param ... Extra arguments to be passed to smoother (e.g, bandwidth).
#' @param full_output TRUE means smoother returns residuals as well as smoothed
#'   series.
#' @seealso \code{\link{process_outcome_model}} for wrapper function for this
#'   method that is easier to use.
#' @return Dataframe with columns corresponding to the simulations.  If
#'   summarize=TRUE, one row per month in original data.  If FALSE, all the
#'   details of all the runs are returned.
#' @example examples/extrapolate_model.R
#' @export
extrapolate_model = function( M0, outcomename, dat, t0, R=400, summarize=FALSE, smooth=FALSE,
                              smoother = smooth_series, full_output = FALSE, fix_parameters = FALSE, ...) {
  # require( tidyverse )

  if ( fix_parameters ) {
    predictions = make_many_predictions_plug( M0, dat=dat, outcomename=outcomename, R=R, t0 = t0 )
  } else {
    predictions = make_many_predictions( M0, dat=dat, outcomename=outcomename, R=R, t0 = t0 )
  }

  # If we are smoothing, smooth all the simulation trajectories
  if ( smooth ) {
    predictions = predictions %>%
      tidyr::nest( data = c(month, Ybar, Ystar) ) %>%
      dplyr::mutate( Ysmooth = purrr::map( data, smoother,
                             outcomename="Ystar", t0 = t0, ... ) ) %>%
      tidyr::unnest(cols = c(data, Ysmooth))
  } else {
    predictions$Ysmooth = NA
  }

  # If we are summarizing, collapse runs into envelope.
  if ( summarize ) {

    if ( !smooth ) {

      p2 = predictions %>% dplyr::group_by( month ) %>%
        dplyr::summarise( Ymin = stats::quantile( Ystar, 0.025, na.rm=TRUE ),
                   Ymax = stats::quantile( Ystar, 0.975, na.rm=TRUE  ),
                   range = (Ymax - Ymin),
                   SE = stats::sd(Ystar, na.rm=TRUE ),
                   Ystar = stats::median( Ystar, na.rm=TRUE  ) )
      p2$Y = dat[[outcomename]]
      p2$Ysmooth = NA
      p2$Ysmooth1 = NA

    } else {

      p2 = predictions %>% dplyr::group_by( month ) %>%
        dplyr::summarise( Ymin = stats::quantile( Ysmooth, 0.025, na.rm=TRUE ),
                   Ymax = stats::quantile( Ysmooth, 0.975, na.rm=TRUE ),
                   range = (Ymax - Ymin),
                   SE = stats::sd(Ysmooth, na.rm=TRUE ),
                   Ysmooth = stats::median( Ysmooth, na.rm=TRUE  ),
                   Ystar = stats::median( Ystar, na.rm=TRUE  ) )

      # Add in smoothed true data line, smoothing the same way as we did with our synthetic lines.
      full.dat = data.frame( month=dat$month, Y = dat[[outcomename]] )
      full.dat$Ysmooth1 = smoother( full.dat, outcomename="Y", t0=t0, ... )

      #full.dat = rename( full.dat, Ystar1 = Ystar, Ysmooth1 = Ysmooth )
      #if ( full_output ) {
      #  full.dat = rename( full.dat, resid1 = resid, residStar1 = residStar )
      #}
      p2 = merge( p2, full.dat, by="month", all=TRUE )
    }

    attr( p2, "model" ) = M0

    p2
  } else {
    predictions
  }
}





