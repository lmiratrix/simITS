##
## (FAKE) New Jersey Example
##

library( tidyverse )
library( simITS )

t0 = -12

# Set up ggplot
library( ggthemes )
my_t = theme_tufte() + theme( legend.position="bottom",
                              legend.direction="horizontal", legend.key.width=unit(1,"cm"),
                              panel.border = element_blank() )
theme_set( my_t )


data( newjersey )
head( newjersey )



#' Helper function to fit the model.  This fits a lagged outcome model to get autoregressive
#' errors. The rest of the code can allow this to be changed without messing
#' other stuff up, as long as there is a 'lag.outcome' variable in the model.
fit.season.model.qtemp =  make.fit.season.model( ~ temperature + Q2 + Q3 + Q4 )

fit.season.model.temp =  make.fit.season.model( ~ temperature  )

fit.season.model.q =  make.fit.season.model( ~ Q2 + Q3 + Q4  )

#fit.season.model.sin =  make.fit.season.model( ~ sin.m + cos.m )
fit.season.model.sin =  make.fit.season.model( ~ 1, no.lag = ~ sin.m + cos.m )

fit.season.model.sintemp =  make.fit.season.model( ~ sin.m + cos.m + Q2 + Q3 + Q4 )


mod = fit.season.model.qtemp( dat = filter( newjersey, month <= t0 ), "Y", lagless=TRUE )
summary( mod )


ggplot( newjersey, aes( month, Y ) ) +
  geom_line() + geom_point( size=0.5) +
  labs( y="Total number of cases", x="month" ) +
  geom_vline( xintercept=c(0,-12), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 )
ggsave( "my_examples/plots/new_jersey_raw.pdf", width=4.5, height=3 )


# Funky version for pretty title slide in talk
ggplot( newjersey, aes( month, Y ) ) +
  geom_line() + geom_point( size=0.5) +
  labs( y="Total number of cases", x="month" ) +
  #  coord_cartesian( ylim=c(0,20000 ) ) +
  geom_hline( yintercept = 0 ) +
  theme_economist()
ggsave( "my_examples/plots/new_jersey_slidetitle.pdf", width=6.5, height=2 )


# How does temp correlate with total number of cases?
newjersey = mutate( newjersey, postpolicy = ifelse( month <= 0, ifelse( month < -12, "pre", "mid" ), "post" ) )
ggplot( newjersey, aes( temperature, Y, pch=postpolicy, col=postpolicy ) ) +
  geom_point( size=3) +
  labs( y="Total number of cases", x="Average temperature in New Jersey" )
ggsave( "my_examples/plots/new_jersey_temp.pdf", width=4, height=3 )



# Make all the lagged covariates based on the functions
newjersey = add.lagged.covariates( newjersey, outcomename = "Y", covariates=fit.season.model.qtemp )
newjersey = add.lagged.covariates( newjersey, outcomename = "Y", covariates=fit.season.model.sin )
head( newjersey )


# Fit the various different seasonality models
nj.pre = filter( newjersey, month < -6 )
newjersey$model.q = predict( fit.season.model.q(nj.pre,"Y", lagless=TRUE), newdata=newjersey )
newjersey$model.qtemp = predict( fit.season.model.qtemp(nj.pre,"Y", lagless=TRUE), newdata=newjersey )
newjersey$model.temp = predict( fit.season.model.temp(nj.pre,"Y", lagless=TRUE), newdata=newjersey )
newjersey$model.sin = predict( fit.season.model.sin(nj.pre,"Y", lagless=TRUE), newdata=newjersey )
newjersey$model.sintemp = predict( fit.season.model.sintemp(nj.pre,"Y", lagless=TRUE), newdata=newjersey )

njplot = newjersey %>% dplyr::select( month, Y, model.q, model.qtemp, model.temp, model.sin, model.sintemp ) %>%
  gather( model.q, model.qtemp, model.temp, model.sin, model.sintemp, key="model", value="Y.hat" )

njplot = filter( njplot, model != "model.sintemp" )
head( njplot )
njplot = mutate( njplot, model = fct_recode( model,
                                             "Quarter"="model.q",
                                             "Quarter+Temp"="model.qtemp",
                                             "Sinusoid"="model.sin",
                                             "Temp" = "model.temp"))
ggplot( njplot, aes( x=month ) ) +
  facet_wrap( ~ model ) +
  geom_line( aes( y=Y ), col="grey" ) + #+ geom_point( aes( y=Y ), col="grey" ) +
  geom_line( aes( y=Y.hat ) )  #, col=model, group=model, lty=model
ggsave( "my_examples/plots/fourmodels.pdf", width = 4, height = 3 )

head( njplot )
njplot = mutate( njplot, resid = Y.hat - Y )
njsub = filter( njplot, month < 0 )
ggplot( njsub, aes( x=month ) ) +
  facet_wrap( ~ model ) +
  geom_hline( yintercept = 0 ) +
  geom_line( aes( y=resid ) )  #, col=model, group=model, lty=model

# Which method generally has the smallest residuals
njsub %>% group_by( model ) %>%
  summarise( sd.resid = sd( resid ) )



######  Fit the models and extrapolate ######

# Fit unsmoothed seasonality model and make envelope
envelope = process.outcome.model( "Y", newjersey, t0=t0, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model.qtemp )

head( envelope )

plt <- make.envelope.graph( envelope, t0=t0 ) +
  #geom_line( aes( y=Ystar ) )  +
  geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 ) +
  coord_cartesian( ylim=c(0,85) )

plt
ggsave( "my_examples/plots/new_jersey.pdf", width=4.75, height=3 )



# Now fit smoothed seasonality model and make envelope
envelope.smooth = process.outcome.model( "Y", newjersey, t0=t0, R = 1000,
                                  summarize = TRUE, smooth=TRUE, smooth_k = 25,
                                  fit.model = fit.season.model.qtemp )

head( envelope.smooth )

plt <- make.envelope.graph( envelope.smooth, t0=t0 ) +
  geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 ) +
  coord_cartesian( ylim=c(0,85) )
plt
ggsave( "my_examples/plots/new_jersey_smoothed.pdf", width=4.75, height=3 )


# Looking at smoothing
if ( FALSE ) {
  M0 = fit.season.model.qtemp( newjersey, "Y" )
  M0full = model.frame( M0, data=newjersey, na.action=NULL )

  smoother = simITS:::make.model.smoother(fit.model = fit.season.model.qtemp, covariates=M0full )
  newjersey$sm1 = smoother( newjersey, t0=t0, "Y", smooth_k = 25 )
  newjersey$sm2 = smoother( newjersey, t0=t0, "Y", smooth_k = 11 )
  gg = newjersey %>% dplyr::select( month, sm1, sm2, Y ) %>%
    gather( sm1, sm2, Y, key="series", value="Y" )
  gg = filter( gg, month > -20 )
  ggplot( gg, aes( month, Y, col=series ) ) +
    geom_line()
  debug( smoother )
}



######  Fit alternate models  ######



# Fit unsmoothed seasonality model and make envelope (no temp)
envelope = process.outcome.model( "Y", newjersey, t0=t0, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model.q )

head( envelope )

plt <- make.envelope.graph( envelope, t0=t0 ) +
  #geom_line( aes( y=Ystar ) )  +
  geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 ) +
  geom_line( aes( y=Ybar ), col="blue" )

plt
ggsave( "my_examples/plots/new_jersey_q.pdf", width=4.75, height=3 )



head( newjersey )
envelope.sin = process.outcome.model( "Y", newjersey, t0=t0, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model.sin )

plt <- make.envelope.graph( envelope.sin, t0=t0 ) +
  geom_line( aes( y=Ystar ) )
plt
ggsave( "my_examples/plots/new_jersey_sin.pdf", width=4.75, height=3 )




envelope.q = process.outcome.model( "Y", newjersey, t0=t0, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model.q )
plt <- make.envelope.graph( envelope.q, t0=t0 ) +
  geom_line( aes( y=Ystar ) )
plt + geom_hline(yintercept= 0 )
ggsave( "my_examples/plots/new_jersey_q.pdf", width=4.75, height=3 )








##### Seeing how autoregression without seasonality modeling works for New Jersey  #####

envelope.sm = process.outcome.model( "Y", newjersey, t0=t0, R = 1000,
                                  summarize = TRUE, smooth=FALSE )

make.envelope.graph( envelope.sm, t0=t0 )  +   geom_line( aes( y=Ystar ) ) +
  geom_hline( yintercept=0 ) +
  coord_cartesian( ylim=c(0,85) )

ggsave( "my_examples/plots/new_jersey_no_season.pdf", width=4.75, height=3 )





