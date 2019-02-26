# New Jersey Example

library( tidyverse )
library( simITS )

# Set up ggplot
library( ggthemes )
my_t = theme_tufte() + theme( legend.position="bottom",
                              legend.direction="horizontal", legend.key.width=unit(1,"cm"),
                              panel.border = element_blank() )
theme_set( my_t )




#' Helper function to fit the model.  This fits a lagged outcome model to get autoregressive
#' errors. The rest of the code can allow this to be changed without messing
#' other stuff up, as long as there is a 'lag.outcome' variable in the model.
fit.season.model.qtemp =  make.fit.season.model( ~ temperature + Q2 + Q3 + Q4 )

fit.season.model.temp =  make.fit.season.model( ~ temperature  )

fit.season.model.q =  make.fit.season.model( ~ Q2 + Q3 + Q4  )

fit.season.model.sin =  make.fit.season.model( ~ tsin1 + tcos1 )

fit.season.model.sintemp =  make.fit.season.model( ~ tsin1 + tcos1 + Q2 + Q3 + Q4 )

data( newjersey )
head( newjersey )


ggplot( newjersey, aes( month, comptot.cs ) ) +
  geom_line() + geom_point( size=0.5) +
  labs( y="Total number of cases", x="month" ) +
#  coord_cartesian( ylim=c(0,20000 ) ) +
  geom_vline( xintercept=c(0,-7), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 )
ggsave( "new_jersey_raw.pdf", width=4.5, height=3 )



# How does temp correlate with total number of cases?
newjersey = mutate( newjersey, postpolicy = ifelse( month <= 0, ifelse( month < -6, "pre", "mid" ), "post" ) )
ggplot( newjersey, aes( temperature, comptot.cs, pch=postpolicy, col=postpolicy ) ) +
  geom_point( size=3) +
  labs( y="Total number of cases", x="Average temperature in New Jersey" )
ggsave( "new_jersey_temp.pdf", width=4, height=3 )



# Fit the four different seasonality models
newjersey = add.lagged.covariates( newjersey, outcomename = "comptot.cs", covariates=fit.season.model.qtemp )
newjersey = add.lagged.covariates( newjersey, outcomename = "comptot.cs", covariates=fit.season.model.sin )
head( newjersey )


nj.pre = filter( newjersey, month < -6 )
newjersey$model.q = predict( fit.season.model.q(nj.pre,"comptot.cs", lagless=TRUE), newdata=newjersey )
newjersey$model.qtemp = predict( fit.season.model.qtemp(nj.pre,"comptot.cs", lagless=TRUE), newdata=newjersey )
newjersey$model.temp = predict( fit.season.model.temp(nj.pre,"comptot.cs", lagless=TRUE), newdata=newjersey )
newjersey$model.sin = predict( fit.season.model.sin(nj.pre,"comptot.cs", lagless=TRUE), newdata=newjersey )
newjersey$model.sintemp = predict( fit.season.model.sintemp(nj.pre,"comptot.cs", lagless=TRUE), newdata=newjersey )

njplot = newjersey %>% dplyr::select( month, comptot.cs, model.q, model.qtemp, model.temp, model.sin, model.sintemp ) %>%
  gather( model.q, model.qtemp, model.temp, model.sin, model.sintemp, key="model", value="Y" )

njplot = filter( njplot, model != "model.sintemp" )
njplot = mutate( njplot, model = fct_recode( model,
                                             "Quarter"="model.q",
                                             "Quarter+Temp"="model.qtemp",
                                             "Sinusoid"="model.sin",
                                             "Temp" = "model.temp"))
ggplot( njplot, aes( x=month ) ) +
  facet_wrap( ~ model ) +
  geom_line( aes( y=comptot.cs ), col="grey" ) + #+ geom_point( aes( y=comptot.cs ), col="grey" ) +
  geom_line( aes( y=Y ) )  #, col=model, group=model, lty=model
ggsave( "my_examples/plots/fourmodels.pdf", width = 4, height = 3 )


njplot = mutate( njplot, resid = Y - comptot.cs )
njsub = filter( njplot, month < 0 )
ggplot( njsub, aes( x=month ) ) +
  facet_wrap( ~ model ) +
  geom_hline( yintercept = 0 ) +
  geom_line( aes( y=resid ) )  #, col=model, group=model, lty=model

njsub %>% group_by( model ) %>%
  summarise( sd.resid = sd( resid ) )

#+
#  geom_hline( yintercept= 0 )

######  Fit the models and extrapolate ######

# Fit unsmoothed seasonality model and make envelope
envelope = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model.qtemp )

head( envelope )

e2 = filter( envelope, month > -25 )
plt <- make.envelope.graph( e2, t0=-7 ) +
  #geom_line( aes( y=Ystar ) )  +
  geom_vline( xintercept=c(0,-7), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 10000 )
#  geom_line( aes( y=Ybar ), col="purple")

plt
ggsave( "my_examples/plots/new_jersey.pdf", width=4.75, height=3 )



# Now fit smoothed seasonality model and make envelope
envelope.sm = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=TRUE,
                                  fit.model = fit.season.model.qtemp )

e2.sm = filter( envelope.sm, month > -25 )

plt <- make.envelope.graph( e2.sm, t0=-7 ) +
  geom_vline( xintercept=c(0,-7), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 10000 )
plt
ggsave( "my_examples/plots/new_jersey_smoothed.pdf", width=4.75, height=3 )



######  Fit alternate models  ######



# Fit unsmoothed seasonality model and make envelope (no temp)
envelope = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = make.fit.season.model( ~ Q2 + Q3 + Q4 ),
                                  covariates = c("Q1","Q2","Q3","Q4") )

head( envelope )

plt <- make.envelope.graph( envelope, t0=-7 ) +
  #geom_line( aes( y=Ystar ) )  +
  geom_vline( xintercept=c(0,-7), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 ) +
  geom_line( aes( y=Ybar ), col="blue" )

plt
ggsave( "new_jersey.pdf", width=4.75, height=3 )







envelope.sin = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model.sin,
                                  covariates = c("tsin1","tcos1" ) )

plt <- make.envelope.graph( envelope, t0=-7 ) +
  geom_line( aes( y=Ystar ) )
plt
ggsave( "new_jersey.pdf", width=4.75, height=3 )


#plt + geom_vline( xintercept = -7, lty=2, col="red" ) +
#  geom_hline( yintercept = -7 )




envelope = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=FALSE,
                                  fit.model = fit.season.model )
plt <- make.envelope.graph( envelope, t0=-7 ) +
  geom_line( aes( y=Ystar ) )
plt + geom_hline(yintercept= 0 )
ggsave( "new_jersey.pdf", width=4.75, height=3 )

#plt + geom_vline( xintercept = -7, lty=2, col="red" ) +
#  geom_hline( yintercept = -7 )








data( newjersey )
head( newjersey )
envelope = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=TRUE,
                                  fit.model = fit.season.model )
plt <- make.envelope.graph( envelope, t0=-7 )
plt

#plt + geom_vline( xintercept = -7, lty=2, col="red" ) +
#  geom_hline( yintercept = -7 )





##### Seeing how autoregression without seasonality modeling works for New Jersey  #####

t0 = -7
envelope.sm = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=FALSE )

make.envelope.graph( envelope.sm, t0=-7 )  +   geom_line( aes( y=Ystar ) ) +
  geom_hline( yintercept=0 )

ggsave( "new_jersey_no_season.pdf", width=4.75, height=3 )


##### Without seasonality, do we have autoregressive structure? #####

# Visual investigation on whether Meck has autoregressive structure (well, actually it does not appear to have such structure )
nj.pre = filter( newjersey, month <= t0 )
mod0 = lm( comptot.cs ~ 1 + month, data=nj.pre )
mod0

# look at autocorrealtion plots on residuals: we have strong autocorrelation
acf( resid( mod0 ) )

# Look at actual and synthetic (fake) plots  (pre-policy only).
mm = nj.pre
mm$synth.compwarr = predict( mod0 ) + sample( resid( mod0 ) )
mm$resid = resid( mod0 )
mm$synth.resid = sample( resid( mod0 ) )
mm = gather( mm, comptot.cs, synth.compwarr, resid, synth.resid, key="outcome", value="Y" )
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
t.obs = count.crosses( resid( mod0) )
t.obs

rps = replicate( 100, {
  count.crosses( sample( resid(mod0) ) )
})
table( rps )
mean( rps <= 17 )


#### Some more stuff #####


envelope = process.outcome.model( "comptot.cs", newjersey, t0=-7, R = 1000,
                                  summarize = TRUE, smooth=FALSE )
head( envelope )
tail( envelope )


# get last pre-policy timepoint
Y.init = filter( envelope, month == t0 )$Y

ggplot( envelope, aes( month ) ) +
  geom_line( aes( y=Y ), alpha = 0.6 ) + geom_point( aes( y=Y ) ) +
  geom_point( x=t0, y=Y.init, col="red" ) +
  geom_ribbon( aes( ymin=Ymin, ymax=Ymax ), alpha=0.2, fill="green" ) +
  geom_vline( xintercept=t0, col="black" )


rng = mean( (envelope$Ymax - envelope$Ymin)[envelope$month > t0 ] )
rng
(envelope$Ymax - envelope.sm$Ymax) / rng

