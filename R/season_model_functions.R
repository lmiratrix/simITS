
#' Default ITS model
#'
#' This fits the model `outcomename ~ lag.outcome + month`, with no
#' covariates.
#'
#' @param dat Dataframe of pre-policy data to fit model to.  Needs a "month" column
#' @param outcomename Outcome of interest
#' @param lagless Boolean, include the lagged outcome, or not?
#' @param ... Extra arguments passed to the lm() call.

#' @export
#' @return A fit model (a `lm` object from a `lm()` call) from fitting a simple
#'   regression of outcome onto month and lagged month.
#'
#' @examples
#' mecklenberg = add_lagged_covariates(mecklenberg, "pbail")
#' meck.pre = filter( mecklenberg, month <= 0 )
#' mod = fit_model_default( meck.pre, "pbail", lagless = TRUE )
#' summary( mod )
#' mod = fit_model_default( meck.pre, "pbail", lagless = FALSE )
#' summary( mod )
fit_model_default = function( dat, outcomename, lagless = FALSE, ... ) {
  monthname = "month"
  
  if ( lagless ) {
    form = paste0( outcomename, " ~ ", monthname )
  } else {
    form = paste0( outcomename, " ~ ", monthname, " + lag.outcome" )
  }
  
  M0 = stats::lm( stats::as.formula( form ), data=dat[-c(1),], ... )
  M0
}

#' Make a fit_model that takes a seasonality component
#'
#' This method returns a function that will fit a model both with and without
#' lagged outcomes.
#'
#' You hand it a formula object specifying the seasonality, e.g., " ~ Q2 + Q3 +
#' Q4", if you have quarterly season effects. This method assumes you want
#' models with a linear month component as well, and will add that and an
#' intercept in automatically.
#'
#' @param formula Formula specifying seasonality.  No outcome or month needed.
#' @param no_lag Formula specifying additional variables to not lag (usually used due
#'   to colinearity of lagged outcomes, such as with a sin and cos component).
#' @return A callable function that takes the arguments of dat, outcomename, and a lagless flag (see,
#'   e.g., the parameters listed in `fit_model_default()`).
#' @seealso fit_model_default for the type of function this method will generate.   
#' @export
#' @examples
#' data( "newjersey")
#' modF = make_fit_season_model( ~ temperature )
#' newjersey = add_lagged_covariates( newjersey, "n.warrant", covariates = c("temperature") )
#' modF( newjersey, "n.warrant" )
make_fit_season_model = function( formula, no_lag = NULL ) {
  
  stopifnot( attr( stats::terms( formula ), "response" ) == 0 )
  
  formula = stats::update.formula( formula, ~ . - month )
  
  vrs = all.vars( formula )
  
  # add in the no_lag elements
  if ( !is.null( no_lag ) ) {
    stopifnot( attr( stats::terms( no_lag ), "response" ) == 0 )
    
    vrno = all.vars( no_lag )
    vrno = paste0( vrno, collapse = " + " )
    formula = stats::update.formula( formula, paste0( "~ . + ", vrno ) )
  }
  
  if ( length( vrs ) > 0 ) {
    vrs
    lgs = paste0( "lag.", vrs, collapse = " + " )
    lgs
    lag.form = stats::update.formula( formula, paste0( "~ . + lag.outcome + ", lgs ) )
  } else {
    lag.form = stats::update.formula( formula, "~ . + lag.outcome" )
  }
  
  fnct = function( dat, outcomename, lagless = FALSE, ... ) {
    if ( lagless ) {
      the.formula = formula
    } else {
      the.formula = lag.form
    }
    the.formula = stats::update( the.formula, stats::as.formula( paste0( outcomename, " ~ 1 + month + ." ) ) )
    
    M0 = stats::lm( the.formula, data=dat[-c(1), ], ...)
    M0
  }
  attr( fnct, "lags" ) = vrs
  attr( fnct, "formula" ) = formula
  attr( fnct, "lag.formula" ) = lag.form
  
  fnct
}

