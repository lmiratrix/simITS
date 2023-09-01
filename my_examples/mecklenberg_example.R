##
## Mecklenberg example used in the methods paper
##
## This file generates all the figures needed for the paper and initial Keynote
## presentations as well.
## 

file_format = "pdf" # jpeg" # eps" # NULL


library( tidyverse )
library( simITS )

data( "mecklenberg")
head( mecklenberg )


FIG_WIDTH = 3.7
FIG_HEIGHT = 2.8


# Number of iterations for simulations
R = 10000
R = 10

# When pre-policy ends
t0 = 0


meck = mutate( mecklenberg, pbail = 100 * pbail )

meck = add_lagged_covariates( meck, outcomename = "pbail", covariates=NULL )
head( meck )
tail( meck )

# Make plots look right to me.
library( ggthemes )
my_t = theme_tufte() + theme( legend.position="bottom",
                              legend.direction="horizontal", legend.key.width=unit(1,"cm"),
                              panel.border = element_blank() )
theme_set( my_t )



#### Fitting simple OLS models to it ####

head( meck )
meck2 = mutate( meck,
                Tx = as.numeric( month > 0 ) )
M.wrong = lm( pbail ~ 1 + month * Tx, data = meck2 )
summary(M.wrong)
confint(M.wrong)
meck2$pred = predict( M.wrong)

ggplot( meck2, aes( x=month, y=pbail)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0.5, xmax=25, fill="lightgray"), col = "lightgray", alpha=0.25) +
  scale_fill_identity(name = "", guide = "none", labels = c('Post Policy era')) +
  geom_hline( yintercept = 0, col="black") +
  geom_line( col="black", lty=1, lwd=0.5) +
  geom_point() +
  scale_x_continuous( breaks = c(-29,-24,-18,-12,-6,1,6,12,18,24)) +
  coord_cartesian(xlim=c(-29.5,24.5), ylim=c(0,100), expand=FALSE) +
  labs( title = " ", y = "Percent cases assigned bail", x = " " ) +
  geom_line( aes( y = pred ), col="red" )


library( nlme )

mod.gls <- gls( pbail ~ 1 + month * Tx, data = meck2, 
                correlation = corARMA(p=1), method="ML" )
summary( mod.gls )

confint( mod.gls )
meck2$predGLS = predict( mod.gls )

ggplot( meck2, aes( x=month, y=pbail)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0.5, xmax=25, fill="lightgray"), col = "lightgray", alpha=0.25) +
  scale_fill_identity(name = "", guide = "none", labels = c('Post Policy era')) +
  geom_hline( yintercept = 0, col="black") +
  geom_line( col="black", lty=1, lwd=0.5) +
  geom_point() +
  scale_x_continuous( breaks = c(-29,-24,-18,-12,-6,1,6,12,18,24)) +
  coord_cartesian(xlim=c(-29.5,24.5), ylim=c(0,100), expand=FALSE) +
  labs( title = " ", y = "Percent cases assigned bail", x = " " ) +
  geom_line( aes( y = predGLS ), col="red" ) +
  geom_line( aes( y = pred ), col="green" )

meck2$predGLS - meck2$pred

arm::se.coef( M.wrong )
summary( mod.gls )
summary( M.wrong )
  
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

ggsave( "my_examples/plots/mech_bail_data.pdf", 
        width = FIG_WIDTH, height = FIG_HEIGHT )
ggsave( paste0( "my_examples/plots/mech_bail_data.", file_format ),
        width = FIG_WIDTH, height = FIG_HEIGHT, device=file_format )

# Breaking the fitting down into parts to examine those regressions
# (Using the utility function from the package)

# With lag outcome
meck.pre = filter( meck, month <= 0 )
mod = fit_model_default( meck.pre, "pbail" )
summary( mod )

# Drop lag (i.e., fit a line to pre-policy data)
mod_lagless = fit_model_default( meck.pre, "pbail", lagless = TRUE )
summary( mod_lagless )




#### Estimating the residual variation breakdown ####


# Frac residual variation due to prior outcome: 7%
coef( mod )
coef( mod )["lag.outcome"]^2


# Our change point
filter( meck, month == t0 )

meck.pre$Yhat = predict( mod, newdata=meck.pre )
meck.pre$Yhat.lagless = predict( mod_lagless, newdata=meck.pre )


# Our predicted line (fully structural and using the lags to help predict)
ggplot( meck.pre, aes( month, pbail ) ) + 
  geom_line() + geom_point() +
  geom_line( aes( y=Yhat ), col="blue" ) +
  geom_line( aes( y=Yhat.lagless, col="red" ) )



#####  Do we have autoregressive structure? #####

# Visual investigation on whether Meck has autoregressive structure (well, actually it does not appear to have such structure )
mod0 = lm( pbail ~ 1 + month, data=meck.pre )
mod0
mod_lagless

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
count_crosses = function( Y ) {
  Y = sign( Y )
  n = length(Y)
  sum( Y[1:(n-1)] != Y[2:n] )
}

mm %>% filter( outcome %in% c("resid", "synth.resid" ) ) %>%
  group_by( outcome ) %>%
  summarise( crosses = count_crosses( Y ) )

rps = replicate( 100, {
  count_crosses( sample( resid(mod0) ) )
})
table( rps )

# Our p-value for autoregressive structure of the residuals.  (We find no evidence of such structure.)
mean( rps <= 13 )



#### Series of demonstration plots of the simulation process  ####

set.seed( 1234 )
predictions = process_outcome_model( "pbail", meck,
                                  t0=t0, R = 10,
                                  summarize = FALSE, smooth=FALSE,
                                  plug_in = TRUE)


head( predictions )

# These are the observed (prepolicy) or simulated (postpolicy) outcomes
ggplot( filter( predictions, month >= t0 ), aes( month, Ystar ) ) +
  geom_line( aes(  group=Run ), alpha=0.5) +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail") +
  coord_cartesian(xlim=c(-29.5,24.5), ylim=c(40,70), expand=FALSE) 

# Used in paper (Figure 2a)
ggsave( paste0( "my_examples/plots/meck_ten_trajectories.", file_format ), 
        width = FIG_WIDTH, height = FIG_HEIGHT, device=file_format )


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
  ggsave( fname, width = FIG_WIDTH, height = FIG_HEIGHT )
}


ggplot( filter( predictions, month >= t0 ), aes( month, Ystar ) ) +
  geom_line( aes(  group=Run ), alpha=0.5) +
  #  geom_point() +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail") +
  #coord_cartesian( xlim=range( meck$month ), ylim=range( meck$pbail) ) 
coord_cartesian(xlim=c(-29.5,24.5), ylim=c(0,100), expand=FALSE) 
  
ggsave( "my_examples/plots/build_out_finished.pdf", width = FIG_WIDTH, height = FIG_HEIGHT )





##### Generate our simulated series (no smoothing)  #####

# Make the envelope from 10,000 trials (incorporating model uncertainty)
envelope = process_outcome_model( "pbail", meck,
                                     t0=t0, R = R,
                                     summarize = TRUE, smooth=FALSE )


# The envelope from 10,000 trials WITHOUT uncertainty.  (This one is WRONG!)
envelope.plug = process_outcome_model( "pbail", meck,
                                       t0=t0, R = R,
                                       summarize = TRUE, smooth=FALSE,
                                       plug_in = TRUE )

nrow( envelope )
head( envelope )
tail( envelope )
#str( envelope )
Y.init = filter( envelope, month == t0 )$Y
#str( envelope )


# Add in pre-policy data for making nice graph
head( envelope.plug )
envelope.plug2 = merge( envelope.plug, meck.pre[c("month","Yhat")], by="month", all.x=TRUE )
head( envelope.plug2 )

# Envelope from plug (too narrow)
make_envelope_graph(envelope = envelope.plug2, t0 = t0) +
  labs( x="month", y="proportion given bail") +
  geom_line( aes(y=Ystar ) )

ggsave( "my_examples/plots/build_out_envelope_plug.pdf", width = FIG_WIDTH, height = FIG_HEIGHT )


# Corrected envelope with uncertainty
# This is Figure 2b from paper
env_graph <- make_envelope_graph(envelope = envelope, t0 = t0) +
  labs( x="month", y="proportion given bail") +
  geom_line( aes(y=Ystar ) ) +
  coord_cartesian(xlim=c(-29.5,24.5), ylim=c(40,70), expand=FALSE) 
env_graph

env_graph +
  geom_line( data=meck2, aes( y = predGLS ), col="black", lty=2 ) 

ggsave( paste0( "my_examples/plots/mech_envelope.", file_format ), width = FIG_WIDTH, height = FIG_HEIGHT,
        device=file_format )






##### Compare keeping and not keeping parameter uncertainty #####


Y.init = filter( envelope, month == t0 )$Y

ggplot( envelope, aes( month ) ) +
  geom_line( aes( y=Y ), alpha = 0.6 ) + geom_point( aes( y=Y ) )  +
  geom_vline( xintercept=t0, col="black" ) +
  geom_point( x=t0, y=Y.init, col="red" ) +
  geom_ribbon( aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="green" ) +
  geom_ribbon( data=envelope.plug, aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="red" ) +
  geom_line( aes( y=Ystar ) )


ggsave( paste0( "my_examples/plots/envelope_plug_in_vs_not.", file_format ), 
        width = FIG_WIDTH, height = FIG_HEIGHT, device=file_format )




#####  Smoothed predictions and associated envelopes #####

MY_SMOOTH = 20

predictions.smooth = process_outcome_model( "pbail", meck,
                                         t0=t0, R = 10,
                                         summarize = FALSE, smooth=TRUE, smooth_k = MY_SMOOTH,
                                         smoother = smooth_series,
                                         post.only = TRUE)
head( predictions.smooth )
ggplot( filter( predictions.smooth, month >= t0 ), aes( month, Ysmooth ) ) +
  geom_line( aes(  group=Run ), alpha=0.5, na.rm=TRUE, ) +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail")
ggsave( "my_examples/plots/ten_smooth_trajectories.pdf", 
        width = FIG_WIDTH, height = FIG_HEIGHT )



# The envelope from R trials using smoothing
envelope.smooth = process_outcome_model( "pbail", meck,
                                  t0=t0, R = R,
                                  summarize = TRUE, smooth=TRUE, smooth_k = MY_SMOOTH,
                                  smoother = smooth_series,
                                  post.only=TRUE )

nrow( envelope.smooth )
head( envelope.smooth )
tail( envelope.smooth )
Y.init = filter( envelope.smooth, month == t0 )$Y

plt = ggplot( envelope.smooth, aes( month ) ) +
  geom_line( aes( y=Y ), alpha = 0.6 ) + geom_point( aes( y=Y ) )  +
  geom_vline( xintercept=t0, col="black" ) +
  geom_point( x=t0, y=Y.init, col="red" ) +
  geom_line( aes( y=Ysmooth ), alpha=0.7, color="green", na.rm=TRUE ) +
  geom_line( aes( y=Ysmooth1 ), color = "red", na.rm=TRUE ) +
  geom_ribbon( aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="green" )
plt
ggsave( "my_examples/plots/build_out_envelope_smooth.pdf", 
        width = FIG_WIDTH, height = FIG_HEIGHT )


# Add in unsmoothed plot to see impact of smoothing.
plt = plt + geom_ribbon( data=envelope, aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="red" )
plt
ggsave( "my_examples/plots/build_out_envelope_smooth_with_old.pdf", 
        width = FIG_WIDTH, height = FIG_HEIGHT )


# Demo of using the utility function for making these graphs.
make_envelope_graph(envelope = envelope.smooth, t0 = t0,
                     xlab="month", ylab="proportion given bail" )



##### Smoothing more vs. less   ######

pds = process_outcome_model( "pbail", meck,
                             t0=t0, R = 20,
                             summarize = FALSE, smooth=TRUE,
                             smooth_k = alpha )
pds



alphas = c( 6, 11, 20, 100 )
preds = purrr::map( alphas, function( alpha ) {
  pds = process_outcome_model( "pbail", meck,
                                                t0=t0, R = 20,
                                                summarize = FALSE, smooth=TRUE,
                                                smoother = smooth_series,
                                                smooth_k = alpha,
                                                post.only = TRUE)
  pds
} )
names( preds ) = alphas
preds = bind_rows( preds, .id="alpha_k" )

head( preds )

ggplot( filter( preds, month >= t0 ), aes( month, Ysmooth ) ) +
  facet_wrap( ~ alpha_k ) +
  geom_line( aes( group=Run, col=alpha_k ), alpha=0.5, na.rm=TRUE) +
  geom_line( data=meck, aes( month, pbail ), col="black" ) +
  geom_point( data=meck, aes( month, pbail ) ) +
  geom_vline( xintercept=t0, col="red" ) +
  labs( x="month", y="proportion given bail")




# Looking at smoothing
if ( FALSE ) {
  M0 = fit_season_model_qtemp( newjersey, "Y" )
  M0full = model.frame( M0, data=newjersey, na.action=NULL )
  
  smoother = simITS:::make_model_smoother(fit.model = fit_season_model_qtemp, covariates=M0full )
  newjersey$sm1 = smoother( newjersey, t0=t0, "Y", smooth_k = 25 )
  newjersey$sm2 = smoother( newjersey, t0=t0, "Y", smooth_k = 11 )
  gg = newjersey %>% dplyr::select( month, sm1, sm2, Y ) %>%
    gather( sm1, sm2, Y, key="series", value="Y" )
  gg = filter( gg, month > -20 )
  ggplot( gg, aes( month, Y, col=series ) ) +
    geom_line()
  debug( smoother )
}





###### Calculating and testing average impact  #######

predictions = process_outcome_model( "pbail", meck,
                                            t0=t0, R = R,
                                            summarize = FALSE, smooth=FALSE )

sstat = aggregate_simulation_results( orig.data = meck, outcomename = "pbail",
                                      predictions = predictions, months = 1:18 )

quantile( sstat$t, c( 0.025, 0.975 ))
sstat$t.obs
sstat$t.obs - quantile( sstat$t, c( 0.025, 0.975 ))

# Look at different range of post-policy months (6 months to 12 months out)
# changes impact estimate.
sstat = aggregate_simulation_results( orig.data = meck, outcomename = "pbail",
                                      predictions = predictions, months = 6:12 )

quantile( sstat$t, c( 0.025, 0.975 ))
sstat$t.obs
sstat$t.obs - quantile( sstat$t, c( 0.025, 0.975 ))



