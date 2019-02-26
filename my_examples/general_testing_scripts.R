# Testing code

library( simITS )
library( tidyverse )

t0 = 0


#### Testing smoothing code to make sure observations are not lost ####
dat = make.fake.data( t.min=-40, t.max=35, t0 = t0)
nrow( dat )
head( dat )

dat = add.lagged.covariates( dat, "Y", covariates = c("Q2","Q3","Q4","temperature" ) )
head( dat )

dat.pre = filter( dat, month <= t0 )

fit.season.model = make.fit.season.model( ~ Q2 + Q3 + Q4 )
M0 = fit.season.model( dat.pre, "Y" )
M0

pds = generate.prediction.sequence( coef( M0 ), sigma(M0), dat=dat, fit0=M0, outcomename="Y", t0=t0 )
head( pds )
tail( pds )
nrow( pds )

ggplot( pds, aes( month ) ) +
  geom_line( aes( y=Ybar ) ) + geom_point( aes( y = Ybar )) +
  geom_line( aes( y=Ystar ), col="blue" )


M0full = model.frame( M0, data=dat, na.action=NULL )
nrow( dat )
nrow( M0full )


# Smooth a single prediction sequence
pds.smooth = smooth.predictions( pds,
                                 t0 = t0, fit0 = M0,
                                 outcomename="Y",
                                 covariates=M0full,
                                 smooth_k = 11)
head( pds.smooth )

pds.smooth = merge( pds, pds.smooth, by="month", all=TRUE )
all( pds.smooth$Y.star.x == pds.smooth$Y.star.y )
head( pds.smooth )

pds2 = gather( pds.smooth, Ybar, Ystar.x, Ysmooth, key="outcome", value="Y" )
head( pds2 )
ggplot( pds2, aes( month, Y, col=outcome ) ) +
  geom_line() + geom_point()



##### Looking at consistancy of the lagged covariate approach  #####

# This looks at the ratio of the lagged coefficents to non-lagged.  We should see a constant ratio.

fit.season.model = make.fit.season.model( ~ temperature + Q2 + Q3 + Q4 )

# Checking when we have lots of data.
# Repeatidly fit our model to the data and look
rps = plyr::rdply( 50, {
  dat = make.fake.data( t.min=-5000, t.max=0, rho=0.6)
  dat = add.lagged.covariates( dat, "Y",
                               covariates = fit.season.model )
  head( dat )
  tail( dat )
  cor( dat[ c("Q2","Q3","Q4","temperature", "lag.Q2","lag.temperature")], use = "complete")
  mod = fit.season.model( dat, "Y" )
  summary( mod )

  cc = coef( mod )
  c.o = cc[3:6]
  c.l = cc[8:11]
  deltas = - c.l / c.o
  deltas
  data.frame( rho.implicit = mean( deltas ) )
} )

nrow( rps )
head( rps )
summary( rps$rho.implicit )
qplot( rps$rho.implicit )


# Examining residuals
# Looking at different prediction sequences  (debugging and exploring code; feel
# free to ignore)
if( FALSE ) {

  # Generate our simulated series (no smoothing)
  predictions = process.outcome.model( "Y", dat, t0=t0, R = 10, summarize = FALSE, smooth=FALSE )
  head( predictions )


  # These are the observed (prepolicy) or simulated (postpolicy) outcomes
  ggplot( predictions, aes( month, Ystar, group=Run ) ) +
    geom_line() +
    geom_vline( xintercept=t0, col="red" ) + geom_point()


  # These are the predictions from the model taking out the autoregressive part
  ggplot( predictions, aes( month, Ybar, group=Run ) ) +
    geom_line() +
    geom_vline( xintercept=t0, col="red" ) + geom_point() +
    geom_line( aes( y=Ystar ), alpha=0.2 )



  # As above, but broken out by facet to see different series more easily
  head( predictions )
  ggplot( predictions, aes( month, Ybar, group=Run ) ) +
    facet_wrap( ~ Run ) +
    geom_line() +
    geom_vline( xintercept=t0, col="red" ) +
    geom_line( aes(y=Ystar), col="blue" )



  # These look at residuals between (possibly) simulated outcome and prediction
  predictions = mutate( predictions, resid = Ystar - Ybar )
  ggplot( predictions, aes( month, resid, col=Run, group=Run ) ) +
    geom_line() +
    geom_vline( xintercept=t0, col="red" ) + geom_point() +
    geom_hline( yintercept= 0 )


  ##
  ## examine smoothing
  ##
  predictions = process.outcome.model( "Y", dat, t0=t0, R = 10, summarize = FALSE, smooth=TRUE, full.output=TRUE)
  head( predictions )

  ggplot( predictions, aes( month, Ystar ) ) +
    geom_line( aes(  group=Run ) ) +
    geom_vline( xintercept=t0, col="red" ) +
    geom_point( data=dat, aes( y=Y ), col="green", size=0.5, alpha=0.5 )

  ggplot( predictions, aes( month, Ysmooth ) ) +
    geom_line( aes(  group=Run ) ) +
    geom_vline( xintercept=t0, col="red" ) +
    geom_point( data=dat, aes( y=Y ), col="green", size=0.5, alpha=0.5 )

  # These are the observed (prepolicy) or simulated (postpolicy) outcomes
  p2 = gather( predictions, Ysmooth, Ystar, key="fit", value="Ystar" )
  head( p2 )
  ggplot( p2, aes( month, Ystar ) ) +
    facet_wrap( ~ Run ) +
    geom_line( aes(  group=Run, colour=fit ) ) + geom_point(aes(  group=Run, colour=fit )) +
    geom_vline( xintercept=t0, col="red" ) +
    geom_point( data=dat, aes( y=Y ), col="green", size=0.5, alpha=0.5 )


  # These are the predictions from the model taking out the autoregressive part
  head( predictions )
  ggplot( predictions, aes( month, Ybar ) ) +
    geom_line( aes(  group=Run ) ) + geom_point(aes(  group=Run )) +
    geom_point( data=dat, aes( y=Y ), col="red" ) +
    geom_vline( xintercept=t0, col="red" )

  # As above, but broken out by facet to see different series more easily
  ggplot( predictions, aes( month, Ybar, group=Run ) ) +
    facet_wrap( ~ Run ) +
    geom_line() + geom_point() +
    geom_line( aes( y=Ystar ), col="red" ) +
    geom_vline( xintercept=t0, col="red" )

  # The residuals
  ggplot( predictions, aes( month, resid, col=Run, group=Run ) ) + geom_line()

  ggplot( predictions, aes( month, residStar, col=Run, group=Run ) ) + geom_line()

  predictions = mutate( predictions, residFinal = Ystar - Ybar )
  ggplot( predictions, aes( month, residFinal, col=Run, group=Run ) ) + geom_line()

  # Degree of smoothing
  predictions = mutate( predictions, residFinal = Ystar - Ybar,
                        residSmooth = Ystar - Ysmooth )
  ggplot( predictions, aes( month, residFinal, col=Run, group=Run ) ) + geom_line()

  ggplot( predictions, aes( month, residSmooth, col=Run, group=Run ) ) + geom_line()

}
