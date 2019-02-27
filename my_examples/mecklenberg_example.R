##
## Mecklenberg example used in the methods paper
##
## This file generates all the graphs needed for the Keynote presentations as well.
##


library( tidyverse )
library( simITS )

data( "mecklenberg")
head( mecklenberg )

t0 = 0

meck = mutate( mecklenberg, pbail = 100 * pbail )

meck = add.lagged.covariates( meck, outcomename = "pbail", covariates=NULL )
head( meck )
tail( meck )

# Make plots look right to me.
library( ggthemes )
my_t = theme_tufte() + theme( legend.position="bottom",
                              legend.direction="horizontal", legend.key.width=unit(1,"cm"),
                              panel.border = element_blank() )
theme_set( my_t )



#### Exploring the Mecklenberg Data ####


ggplot( meck, aes( x=month, y=pbail)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0.5, xmax=25, fill="lightgray"), col = "lightgray", alpha=0.25) +
  scale_fill_identity(name = "", guide = "none", labels = c('Post Policy era')) +
  geom_hline( yintercept = 0, col="black") +
  geom_line( col="black", lty=1, lwd=0.5) +
  geom_point() +
  scale_x_continuous( breaks = c(-29,-24,-18,-12,-6,1,6,12,18,24)) +
  coord_cartesian(xlim=c(-29.5,24.5), ylim=c(0,100), expand=FALSE) +
  labs( title = " ", y = "Percent cases assigned bail", x = " " )


# Breaking the fitting down into parts to examine those regressions
# (Using the utility function from the package)

# With lag outcome
meck.pre = filter( meck, month <= 0 )
mod = fit.model.default( meck.pre, "pbail" )
summary( mod )

# Drop lag (i.e., fit a line to pre-policy data)
mod.lagless = fit.model.default( meck.pre, "pbail", lagless = TRUE )
summary( mod.lagless )

##
## Estimating the residual variation breakdown
##

# Frac residual variation due to prior outcome: 7%
coef( mod )
coef( mod )["lag.outcome"]^2


# Our change point
filter( meck, month == t0 )

meck.pre$Yhat = predict( mod, newdata=meck.pre )
meck.pre$Yhat.lagless = predict( mod.lagless, newdata=meck.pre )


# Our predicted line (fully structural and using the lags to help predict)
ggplot( meck.pre, aes( month, pbail ) ) + 
  geom_line() + geom_point() +
  geom_line( aes( y=Yhat ), col="blue" ) +
  geom_line( aes( y=Yhat.lagless, col="red" ) )



#####  Do we have autoregressive structure? #####

# Visual investigation on whether Meck has autoregressive structure (well, actually it does not appear to have such structure )
mod0 = lm( pbail ~ 1 + month, data=meck.pre )
mod0
mod.lagless

# look at autocorrealtion plots
acf( meck.pre$pbail)
acf( resid( mod0 ) )

# Look at actual and synthetic (fake) plots of mecklenberg (pre-policy only).
mm = meck.pre
mm$synth.pbail = predict( mod0 ) + sample( resid( mod0 ) )
mm$resid = resid( mod0 )
mm$synth.resid = sample( resid( mod0 ) )
mm = gather( mm, pbail, synth.pbail, resid, synth.resid, key="outcome", value="Y" )
head( mm )
table( mm$outcome )
ggplot( mm, aes( month, Y ) ) +
  facet_wrap( ~ outcome, scales="free") +
  geom_line() + geom_smooth(method="lm", se=FALSE)


# Perm test for how often crosses 0 line
count.crosses = function( Y ) {
  Y = sign( Y )
  n = length(Y)
  sum( Y[1:(n-1)] != Y[2:n] )
}

mm %>% filter( outcome %in% c("resid", "synth.resid" ) ) %>%
  group_by( outcome ) %>%
  summarise( crosses = count.crosses( Y ) )

rps = replicate( 100, {
  count.crosses( sample( resid(mod0) ) )
})
table( rps )

# Our p-value for autoregressive structure of the residuals.  (We find no evidence of such structure.)
mean( rps <= 13 )



#### Series of demonstration plots of the simulation process  ####

set.seed( 1234 )
predictions = process.outcome.model( "pbail", meck,
                                  t0=t0, R = 10,
                                  summarize = FALSE, smooth=FALSE,
                                  plug.in = TRUE)


head( predictions )

# These are the observed (prepolicy) or simulated (postpolicy) outcomes
ggplot( filter( predictions, month >= t0 ), aes( month, Ystar ) ) +
  geom_line( aes(  group=Run ), alpha=0.5) +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail")
ggsave( "my_examples/plots/meck_ten_trajectories.pdf", width=7, height=4 )


# Building up the plots of how a sequence is generated
set.seed( 1019 )
table( predictions$Run )
run1 = filter( predictions, Run==2 )

cuts = c( 0, 1, 2, 3, 5, 100 )
plt = NULL
for ( ct in cuts ) {

  run1a = filter( run1, month <= ct )

  plt <- ggplot( run1a, aes( month, Ystar ) ) +
    geom_vline( xintercept=t0, col="red" ) +
    geom_line( aes( group=Run ), col="grey") +
    geom_point( col="grey" ) +
    labs( x="month", y="proportion given bail") +
    coord_cartesian( xlim=range( meck$month ), ylim=range( meck$pbail) )

  fname = paste0( "my_examples/plots/build_out_", ct, ".pdf" )
  print( plt )
  ggsave( fname, width=4, height=3 )
}


ggplot( filter( predictions, month >= t0 ), aes( month, Ystar ) ) +
  geom_line( aes(  group=Run ), alpha=0.5) +
  #  geom_point() +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail")
ggsave( "my_examples/plots/build_out_finished.pdf", width=4, height=3 )




##### Generate our simulated series (no smoothing)  #####

# Make the envelope from 10,000 trials (incorporating model uncertainty)
envelope = process.outcome.model( "pbail", meck,
                                     t0=t0, R = 10000,
                                     summarize = TRUE, smooth=FALSE )


# The envelope from 10,000 trials WITHOUT uncertainty.  (This one is WRONG!)
envelope.plug = process.outcome.model( "pbail", meck,
                                       t0=t0, R = 10000,
                                       summarize = TRUE, smooth=FALSE,
                                       plug.in = TRUE )

nrow( envelope )
head( envelope )
tail( envelope )
str( envelope )
Y.init = filter( envelope, month == t0 )$Y
str( envelope )


# Add in pre-policy data for making nice graph
head( envelope.plug )
envelope.plug2 = merge( envelope.plug, meck.pre[c("month","Yhat")], by="month", all.x=TRUE )

make.envelope.graph(envelope = envelope.plug2, t0 = t0) +
  labs( x="month", y="proportion given bail") +
  geom_line( aes(y=Ystar ) )
#  geom_line( aes(y=Yhat ) )


ggsave( "my_examples/plots/build_out_envelope.pdf", width=4, height=3 )







##### Compare keeping and not keeping parameter uncertainty #####


Y.init = filter( envelope, month == t0 )$Y

ggplot( envelope, aes( month ) ) +
  geom_line( aes( y=Y ), alpha = 0.6 ) + geom_point( aes( y=Y ) )  +
  geom_vline( xintercept=t0, col="black" ) +
  geom_point( x=t0, y=Y.init, col="red" ) +
  geom_ribbon( aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="green" ) +
  geom_ribbon( data=envelope.plug, aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="red" ) +
  geom_line( aes( y=Ystar ) )


ggsave( "my_examples/plots/envelope_plug_in_vs_not.pdf", width=4, height=3 )




#####  Smoothed predictions and associated envelopes #####



predictions.smooth = process.outcome.model( "pbail", meck,
                                         t0=t0, R = 10,
                                         summarize = FALSE, smooth=TRUE,
                                         smoother = smooth.series,
                                         post.only = TRUE)
head( predictions.smooth )
ggplot( filter( predictions.smooth, month >= t0 ), aes( month, Ysmooth ) ) +
  geom_line( aes(  group=Run ), alpha=0.5) +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail")
ggsave( "my_examples/plots/ten_smooth_trajectories.pdf", width=4, height=3 )



# The envelope from 10,000 trials using smoothing
envelope.smooth = process.outcome.model( "pbail", meck,
                                  t0=t0, R = 10000,
                                  summarize = TRUE, smooth=TRUE,
                                  smoother = smooth.series,
                                  post.only=TRUE )

nrow( envelope.smooth )
head( envelope.smooth )
tail( envelope.smooth )
Y.init = filter( envelope.smooth, month == t0 )$Y

plt = ggplot( envelope.smooth, aes( month ) ) +
  geom_line( aes( y=Y ), alpha = 0.6 ) + geom_point( aes( y=Y ) )  +
  geom_vline( xintercept=t0, col="black" ) +
  geom_point( x=t0, y=Y.init, col="red" ) +
  geom_line( aes( y=Ysmooth ), alpha=0.7, color="green" ) +
  geom_line( aes( y=Ysmooth1 ), color = "red" ) +
  geom_ribbon( aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="green" )
plt
ggsave( "my_examples/plots/build_out_envelope_smooth.pdf", width=4, height=3 )


# Add in unsmoothed plot to see impact of smoothing.
plt = plt + geom_ribbon( data=envelope, aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="red" )
plt
ggsave( "my_examples/plots/build_out_envelope_smooth_with_old.pdf", width=4, height=3 )


# Demo of using the utility function for making these graphs.
make.envelope.graph(envelope = envelope.smooth, t0 = t0,
                     xlab="month", ylab="proportion given bail" )




###### Calculating and testing average impact  #######

predictions = process.outcome.model( "pbail", meck,
                                            t0=t0, R = 1000,
                                            summarize = FALSE, smooth=FALSE )

sstat = aggregate.simulation.results( orig.data = meck, outcomename = "pbail",
                                      predictions = predictions, months = 1:18 )

quantile( sstat$t, c( 0.025, 0.975 ))
sstat$t.obs

# Look at different range of post-policy months (6 months to 18 months out)
# changes impact estimate.
sstat = summarize.simulation.results( orig.data = meck, outcomename = "pbail",
                                      predictions = predictions, months = 6:18 )

quantile( sstat$t, c( 0.025, 0.975 ))
sstat$t.obs



