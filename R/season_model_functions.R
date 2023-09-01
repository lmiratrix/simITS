
#' Default ITS model
#'
#' This fits the model `outcomename ~ lag.outcome + time`, with no
#' covariates.
#'
#' @param dat Dataframe of pre-policy data to fit model to.  Needs a "time" column
#' @param outcomename Outcome of interest
#' @param lagless Boolean, include the lagged outcome, or not?
#' @param ... Extra arguments passed to the lm() call.

#' @export
#' @return A fit model (a `lm` object from a `lm()` call) from fitting a simple
#'   regression of outcome onto time and lagged time.
#'
#' @examples
#' mecklenberg = add_lagged_covariates(mecklenberg, "pbail")
#' meck.pre = filter( mecklenberg, month <= 0 )
#' mod = fit_model_default( meck.pre, "pbail", "month", lagless = TRUE )
#' summary( mod )
#' mod = fit_model_default( meck.pre, "pbail", "month", lagless = FALSE )
#' summary( mod )
fit_model_default = function( dat, outcomename, timename = "time", lagless = FALSE,  ... ) {
  
  
  if ( lagless ) {
    form = paste0( outcomename, " ~ ", timename )
  } else {
    form = paste0( outcomename, " ~ ", timename, " + lag.outcome" )
  }
  
  M0 = stats::lm( stats::as.formula( form ), data=dat[-c(1),], ... )
  M0
}









#' Make a fit_model function that takes a seasonality component
#'
#' This method returns a function that will fit a model both with and
#' without lagged outcomes.
#'
#' You hand it a formula object specifying the seasonality, e.g., " ~
#' Q2 + Q3 + Q4", if you have quarterly season effects. This method
#' assumes you want models with a linear time component as well, and
#' will add that and an intercept in automatically.
#'
#' It gives you a function back, that you can use to analyze data.
#'
#' @param formula Formula specifying seasonality.  No outcome or time
#'   needed.
#' @param no_lag Formula specifying additional covariates that should
#'   be included, but without lag (usually used due to colinearity of
#'   lagged outcomes, such as with a sin and cos component).
#' @inheritParams make_fit_model   
#' @return Callable function.  See make_fit_model.
#' @seealso make_fit_model for the type of function this method
#'   will generate.
#' @export
#' @examples
#' data( "newjersey")
#' modF = make_fit_season_model( ~ temperature, timename = "month" )
#' newjersey = add_lagged_covariates( newjersey, "n.warrant", covariates = c("temperature") )
#' modF( newjersey, "n.warrant" )
make_fit_season_model = function( formula, no_lag = NULL ) {
  
  stopifnot( attr( stats::terms( formula ), "response" ) == 0 )
  
  
  vrs = all.vars( formula )
  
  # add in the no_lag elements
  if ( !is.null( no_lag ) ) {
    stopifnot( attr( stats::terms( no_lag ), "response" ) == 0 )
    
    vrno = all.vars( no_lag )
    vrno = paste0( vrno, collapse = " + " )
    formula = stats::update.formula( formula, paste0( "~ . + ", vrno ) )
    
    # Anything explicitly told to not lag, remove from core formula.
    vrs = setdiff( vrs, vrno )
    
  }
  
  
  if ( length( vrs ) > 0 ) {
    vrs
    lgs = paste0( "lag.", vrs, collapse = " + " )
    lgs
    lag.form = stats::update.formula( formula, paste0( "~ . + lag.outcome + ", lgs ) )
  } else {
    lag.form = stats::update.formula( formula, "~ . + lag.outcome" )
  }
  
  fnct = function( dat, outcomename, timename = "time", lagless = FALSE, ... ) {
    if ( lagless ) {
      the.formula = formula
    } else {
      the.formula = lag.form
    }
    
    # Drop the time var, if present, from the core formula
    the.formula = stats::update.formula( the.formula, as.formula( paste0( "~ . - ", timename ) ) )
    
    # Add in the time
    the.formula = stats::update( the.formula, stats::as.formula( paste0( outcomename, " ~ 1 + ", timename, " + ." ) ) )
    
    M0 = stats::lm( the.formula, data=dat[-c(1), ], ...)
    M0
  }
  
  attr( fnct, "lags" ) = vrs
  attr( fnct, "formula" ) = formula
  attr( fnct, "lag.formula" ) = lag.form
  
  fnct
}

