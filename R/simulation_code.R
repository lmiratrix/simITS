


#' Make fake data for testing purposes.
#'
#' Has heavy seasonality, and an extra bump at 12 months post-policy
#'
#' @export
make.fake.data = function( t.min = -40, t.max = 9, t0 = 0, rho = 0.50, sd.omega = 1,
                           coef.line = c( 20, 0.05 ),
                           coef.q = c( 1.0, 0, -1.0, 0 ),
                           coef.temp = 0.10,
                           coef.sin = c( 0, 0 ),
                           coef.tx = c( 0, 0.25, 5 ) ) {
  require( tidyverse )

  # Make some fake data
  dat = data.frame( month = t.min:t.max )
  N = nrow( dat )
  dat = mutate( dat, temperature = 54 + 35 * sin( 2 * pi * (month + 7) / 12 + rnorm( n(), 0, 0.5 ) ),
                sin.m = sin( 2 * pi * month / 12 ),
                cos.m = cos( 2 * pi * month / 12 ),
                Q1 = 0 + (month %%12 < 3),
                Q2 = 0 + ((month %%12 < 6) & !Q1),
                Q3 = 0 + ((month %%12 < 9) & !Q1 & !Q2),
                Q4 = 0 + (!Q1 & !Q2 & !Q3),
                post = month > t0 )

  # make outcome
  dmat = model.matrix( ~ 1 + month + Q1 + Q2 + Q3 + Q4 + temperature + sin.m + cos.m, data=dat )
  dat$Ystr0 = as.numeric( dmat %*% c( coef.line, coef.q, coef.temp, coef.sin ) )
  #Ypost = model.matrix( ~ 1 + I(month-t0), data=dat )
  dat$Ystr = dat$Ystr0 + as.numeric( with( dat, coef.tx[[1]] * post + coef.tx[[2]] * post * (month-t0) + coef.tx[[3]] * (month > t0+12) ) )

  # make autoregressive residual
  eps = rnorm( N, mean = 0, sd = sd.omega )
  for ( i in 2:N ) {
    eps[i] = rho * eps[i-1] + eps[i]
  }
  #plot( eps, type="b" )

  dat$Y = dat$Ystr + eps

  dat
}


if ( FALSE ) {

  df = make.fake.data( t.min = -80, t.max = 12, t0 = 0 )
  ggplot( df, aes( month, Y ) ) +
    geom_line() +
    geom_line( aes( y=Ystr ), col="red" )


  df = make.fake.data( t.min = -80, t.max = 12, t0 = 0, sd.omega = 3,
                       rho = 0)
  ggplot( df, aes( month, Y ) ) +
    geom_line() +
    geom_line( aes( y=Ystr ), col="red" )



  t0=-12

  df = make.fake.data( t.min = -80, t.max = 12, t0 = t0, sd.omega = 3,
                       rho = 0,
                       coef.line = c(50, 0.05 ),
                       coef.temp = 0,
                       coef.sin = c( 5, 0 ),
                       coef.tx = c( 0, 0.5, 5 ) )
  ggplot( df, aes( month, Y ) ) +
    geom_line() +
    geom_line( aes( y=Ystr ), col="red" ) +
    geom_line( aes( y=Ystr0 ), col="green" ) +
    geom_hline( yintercept = 0)


  fit.season.model.qtemp =  make.fit.season.model( ~  Q2 + Q3 + Q4 )

  # Fit unsmoothed seasonality model and make envelope
  envelope = process.outcome.model( "Y", df, t0=t0, R = 1000,
                                    summarize = TRUE, smooth=FALSE,
                                    fit.model = fit.season.model.qtemp )

  head( envelope )

  plt <- make.envelope.graph( envelope, t0=t0 ) +
    geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
    geom_hline( yintercept = 0 )

  plt


  # Now fit smoothed seasonality model and make envelope
  envelope.smooth = process.outcome.model( "Y", df, t0=t0, R = 1000,
                                           summarize = TRUE, smooth=TRUE, smooth_k = 25,
                                           fit.model = fit.season.model.qtemp )

  head( envelope.smooth )

  plt <- make.envelope.graph( envelope.smooth, t0=t0 ) +
    geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
    geom_hline( yintercept = 0 ) +
    geom_ribbon( data=envelope, aes( ymin=Ymin, ymax=Ymax ), alpha=0.1, fill="red" )

  plt


}

