


#' Make fake data for testing purposes.
#'
#' Defaults have heavy seasonality, and an extra bump in impact kicks in at 12
#' months post-policy.
#'
#' @param t_min Index of first month
#' @param t_max Index of last month
#' @param t0 Last pre-policy time point
#' @param rho Autocorrelation
#' @param sd.omega Standard deviation of the true residual
#' @param coef_line Intercept and slope of the main trendline (list of 2).
#' @param coef_q Coefficients for the four quarters (list of 4).
#' @param coef_temp Coefficient for temperature.
#' @param coef_sin Coefficents for sin and cos features (list of 2)
#' @param coef_tx Coefficient for treatment post-policy (list of 3, initial
#'   offset, initial slope, additional slope past 12 months).  Treatment is a
#'   piecewise linear function.
#'
#' @return A \code{data.frame} having \code{month} , \code{temperature} ,
#'   \code{sin.m} , \code{cos.m} , \code{Q1}, \code{Q2} , \code{Q3}, \code{Q4},
#'   \code{post} , \code{Ystr0} , \code{Ystr} , \code{Y}
#'
#' @examples
#' fdat = generate_fake_data(-100,100, rho = 0.95, coef_q=c(0,0,0,0), coef_temp = 0)
#' plot( fdat$month, fdat$Y, type="l" )
#' fdat2 = generate_fake_data(-100, 100, rho = 0.0, coef_q=c(0,0,0,0), coef_temp = 0)
#' plot( fdat$month, fdat2$Y, type="l" )
#' @export
generate_fake_data = function( t_min = -40, t_max = 9, t0 = 0, rho = 0.50, sd.omega = 1,
                           coef_line = c( 20, 0.05 ),
                           coef_q = c( 1.0, 0, -1.0, 0 ),
                           coef_temp = 0.10,
                           coef_sin = c( 0, 0 ),
                           coef_tx = c( 0, 0.25, 5 ) ) {
  # require( tidyverse )

  #initial check for input parametes
  stopifnot(is.numeric(t_min), is.numeric(t_max), is.numeric(t0), is.numeric(rho), is.numeric(sd.omega),
		is.numeric(coef_line), is.numeric(coef_q) ,is.numeric(coef_temp) ,is.numeric(coef_sin) ,
		is.numeric(coef_tx))
  # Make some fake data
  dat = data.frame( month = t_min:t_max )
  N = nrow( dat )
  dat = mutate( dat, temperature = 54 + 35 * sin( 2 * pi * (month + 7) / 12 + stats::rnorm( dplyr::n(), 0, 0.5 ) ),
                sin.m = sin( 2 * pi * month / 12 ),
                cos.m = cos( 2 * pi * month / 12 ),
                Q1 = 0 + (month %%12 < 3),
                Q2 = 0 + ((month %%12 < 6) & !Q1),
                Q3 = 0 + ((month %%12 < 9) & !Q1 & !Q2),
                Q4 = 0 + (!Q1 & !Q2 & !Q3),
                post = month > t0 )

  # make outcome
  dmat = stats::model.matrix( ~ 1 + month + Q1 + Q2 + Q3 + Q4 + temperature + sin.m + cos.m, data=dat )
  dat$Ystr0 = as.numeric( dmat %*% c( coef_line, coef_q, coef_temp, coef_sin ) )
  #Ypost = stats::model.matrix( ~ 1 + I(month-t0), data=dat )
  dat$Ystr = dat$Ystr0 + as.numeric( with( dat, coef_tx[[1]] * post + coef_tx[[2]] * post * (month-t0) + coef_tx[[3]] * (month > t0+12) ) )

  # make autoregressive residual
  eps = stats::rnorm( N, mean = 0, sd = sd.omega )
  for ( i in 2:N ) {
    eps[i] = rho * eps[i-1] + eps[i]
  }
  #plot( eps, type="b" )

  dat$Y = dat$Ystr + eps

  dat
}


if ( FALSE ) {

  df = generate_fake_data( t_min = -80, t_max = 12, t0 = 0 )
  ggplot2::facet_wrapggplot( df, ggplot2::facet_wrapaes( month, Y ) ) +
    ggplot2::geom_line() +
    ggplot2::geom_line( ggplot2::aes( y=Ystr ), col="red" )


  df = generate_fake_data( t_min = -80, t_max = 12, t0 = 0, sd.omega = 3,
                       rho = 0)
  ggplot2::ggplot( df, aes( month, Y ) ) +
    ggplot2::geom_line() +
    ggplot2::geom_line( ggplot2::aes( y=Ystr ), col="red" )



  t0=-12

  df = generate_fake_data( t_min = -80, t_max = 12, t0 = t0, sd.omega = 3,
                       rho = 0,
                       coef_line = c(50, 0.05 ),
                       coef_temp = 0,
                       coef_sin = c( 5, 0 ),
                       coef_tx = c( 0, 0.5, 5 ) )
  ggplot2::ggplot( df, ggplot2::aes( month, Y ) ) +
    ggplot2::geom_line() +
    ggplot2::geom_line( ggplot2::aes( y=Ystr ), col="red" ) +
    ggplot2::geom_line( ggplot2::aes( y=Ystr0 ), col="green" ) +
    ggplot2::geom_hline( yintercept = 0)


  fit.season.model.qtemp =  make_fit_season_model( ~  Q2 + Q3 + Q4 )

  # Fit unsmoothed seasonality model and make envelope
  envelope = process_outcome_model( "Y", df, t0=t0, R = 1000,
                                    summarize = TRUE, smooth=FALSE,
                                    fit_model = fit.season.model.qtemp )

  head( envelope )

  plt <- make_envelope_graph( envelope, t0=t0 ) +
    geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
    geom_hline( yintercept = 0 )

  plt


  # Now fit smoothed seasonality model and make envelope
  envelope.smooth = process_outcome_model( "Y", df, t0=t0, R = 1000,
                                           summarize = TRUE, smooth=TRUE, smooth_k = 25,
                                           fit_model = fit.season.model.qtemp )

  head( envelope.smooth )

  plt <- make_envelope_graph( envelope.smooth, t0=t0 ) +
    ggplot2::geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
    ggplot2::geom_hline( yintercept = 0 ) +
    ggplot2::geom_ribbon( data=envelope, ggplot2::aes( ymin=Ymin, ymax=Ymax ), alpha=0.1, fill="red" )

  plt


}


