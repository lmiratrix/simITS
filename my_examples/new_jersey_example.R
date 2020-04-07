##
## New Jersey Example
##
## This generates all plots used in the methods paper.
## 



library( tidyverse )
library( simITS )

FIG_WIDTH = 3.7
FIG_HEIGHT = 2.8

R = 10000
R = 10

t0 = -8

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
fit_season_model_qtemp =  make_fit_season_model( ~ temperature + Q2 + Q3 + Q4 )

fit_season_model_temp =  make_fit_season_model( ~ temperature  )

fit_season_model_q =  make_fit_season_model( ~ Q2 + Q3 + Q4  )

#fit_season_model_sin =  make_fit_season_model( ~ sin.m + cos.m )
fit_season_model_sin =  make_fit_season_model( ~ 1, no_lag = ~ sin.m + cos.m )

fit_season_model_sintemp =  make_fit_season_model( ~ sin.m + cos.m + Q2 + Q3 + Q4 )


mod = fit_season_model_qtemp( dat = filter( newjersey, month <= t0 ), "n.warrant", lagless=TRUE )
summary( mod )


# Make all the lagged covariates based on the functions
newjersey = add_lagged_covariates( newjersey, outcomename = "n.warrant", covariates=fit_season_model_qtemp )
newjersey = add_lagged_covariates( newjersey, outcomename = "n.warrant", covariates=fit_season_model_sin )
head( newjersey )



#### Explore the raw data ####


ggplot( newjersey, aes( x=month, y=n.warrant)) +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=0.5, xmax=max( newjersey$month), fill="gray"), col = "gray") +
  geom_rect(aes( ymin=-Inf, ymax=Inf, xmin=t0+0.5, xmax=0.5, fill="lightgray"), col = "lightgray") +
  scale_fill_identity(name = "", guide = "none", labels = c('Post Policy era')) +
  geom_hline( yintercept = 0, col="black") +
  geom_line( col="black", lty=1, lwd=0.5) +
  geom_point() +
 coord_cartesian( ylim=c(0,7000), expand=FALSE ) +
  labs( title = " ", y = "Number warrant arrests", x = "Month" )
ggsave( "my_examples/plots/new_jersey_raw.pdf", width=FIG_WIDTH, height=FIG_HEIGHT )


# Funky version of the prior plot to make a pretty/interesting title slide in talk
ggplot( newjersey, aes( month, n.warrant ) ) +
  geom_line() + geom_point( size=0.5) +
  labs( y="Total number of cases", x="month" ) +
  #  coord_cartesian( ylim=c(0,20000 ) ) +
  geom_hline( yintercept = 0 ) +
  theme_economist()
ggsave( "my_examples/plots/new_jersey_slidetitle.pdf", width=6.5, height=2 )


# How does temp correlate with total number of cases?
newjersey = mutate( newjersey, postpolicy = 
                      factor( ifelse( month <= 0, ifelse( month < -8, "Pre", "Rollout" ), "Post" ), levels=c("Pre", "Rollout", "Post") ) )
ggplot( newjersey, aes( temperature, n.warrant, pch=postpolicy, col=postpolicy ) ) +
  geom_point( size=1) +
  labs( y="Total number of cases", x="Average temperature in New Jersey", col="Period:", pch="Period:")
ggsave( "my_examples/plots/new_jersey_temp.pdf", width=FIG_WIDTH, height=FIG_HEIGHT )





#### Fit seasonality models (no autocorrelation) ####


# Fit the various different seasonality models (without lags, to see how well models fit)
nj.pre = filter( newjersey, month < -6 )
newjersey$model.q = predict( fit_season_model_q(nj.pre,"n.warrant", lagless=TRUE), newdata=newjersey )
newjersey$model.qtemp = predict( fit_season_model_qtemp(nj.pre,"n.warrant", lagless=TRUE), newdata=newjersey )
newjersey$model.temp = predict( fit_season_model_temp(nj.pre,"n.warrant", lagless=TRUE), newdata=newjersey )
newjersey$model.sin = predict( fit_season_model_sin(nj.pre,"n.warrant", lagless=TRUE), newdata=newjersey )
newjersey$model.sintemp = predict( fit_season_model_sintemp(nj.pre,"n.warrant", lagless=TRUE), newdata=newjersey )

njplot = newjersey %>% dplyr::select( month, n.warrant, model.q, model.qtemp, model.temp, model.sin, model.sintemp ) %>%
  gather( model.q, model.qtemp, model.temp, model.sin, model.sintemp, key="model", value="Y.hat" )

head( njplot )
njplot = mutate( njplot, model = fct_recode( model,
                                             "Quarter"="model.q",
                                             "Quarter+Temp"="model.qtemp",
                                             "Sinusoid"="model.sin",
                                             "Temp" = "model.temp",
                                             "Sin+Temp" = "model.sintemp"))

njplot4 = filter( njplot, model != "Sin+Temp", month < -6 )

ggplot( njplot4, aes( x=month ) ) +
  facet_wrap( ~ model ) +
  geom_line( aes( y=n.warrant ), col="grey" ) + #+ geom_point( aes( y=n.warrant ), col="grey" ) +
  geom_line( aes( y=Y.hat ) ) +  #, col=model, group=model, lty=model
  labs( x="month", y="# warrants" )
ggsave( "my_examples/plots/fourmodels.pdf", width=FIG_WIDTH, height=FIG_HEIGHT )

ggplot( filter( njplot4, month <= t0 ), aes( x=month ) ) +
  facet_wrap( ~ model, nrow = 1 ) +
  geom_line( aes( y=n.warrant ), col="grey" ) + #+ geom_point( aes( y=n.warrant ), col="grey" ) +
  geom_line( aes( y=Y.hat ) ) +  #, col=model, group=model, lty=model
  labs( x="month", y="# warrants" )
ggsave( "my_examples/plots/fourmodels_row.pdf", width = 10, height = 2 )


# Look at residuals after the model fit
head( njplot )
njplot = mutate( njplot, resid = Y.hat - n.warrant )
njsub = filter( njplot, month < 0 )
ggplot( filter( njsub, model != "Sin+Temp" ), 
        aes( x=month ) ) +
  facet_wrap( ~ model ) +
  geom_hline( yintercept = 0 ) +
  geom_line( aes( y=resid ) )  #, col=model, group=model, lty=model

# Which method generally has the smallest residuals
njsub %>% group_by( model ) %>%
  summarise( sd.resid = sd( resid ) )



######  Fit the models and extrapolate ######


save.nj.plot = function( envelope, filename ) {
  
  plt <- make_envelope_graph( filter( envelope, month > -30 ), t0=t0 ) +
    #geom_line( aes( y=Ystar ) )  +
    geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
    geom_hline( yintercept = 0 ) 
  plt
  ggsave( paste0( "my_examples/plots/", filename, ".pdf" ), plot = plt, width=FIG_WIDTH, height=FIG_HEIGHT )
  
  plt
}



# Fit unsmoothed seasonality model and make envelope
envelope = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                  summarize = TRUE, smooth=FALSE,
                                  fit_model = fit_season_model_qtemp )

head( envelope )
save.nj.plot( envelope, "new_jersey")



# Now fit smoothed seasonality model and make envelope with default model
envelope.smooth.base = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                             summarize = TRUE, smooth=TRUE, smooth_k = 11,
                                             fit_model = fit_season_model_qtemp )
save.nj.plot( envelope.smooth.base, "new_jersey_smoothed_base" )


# Now fit smoothed seasonality model and make envelope with sin smoother
smoother = make_model_smoother( fit_model = fit_season_model_sin, covariates = newjersey )
envelope.smooth.sin = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                  summarize = TRUE, smooth=TRUE, smoother = smoother, smooth_k = 11,
                                  fit_model = fit_season_model_qtemp )

head( envelope.smooth.sin )

save.nj.plot( envelope.smooth.sin, "new_jersey_smoothed_sin" )



# Looking at smoothing
if ( FALSE ) {
  M0 = fit_season_model_qtemp( newjersey, "n.warrant" )
  M0full = model.frame( M0, data=newjersey, na.action=NULL )

  smoother = simITS:::make_model_smoother(fit_model = fit_season_model_qtemp, covariates=M0full )
  newjersey$sm1 = smoother( newjersey, t0=t0, "n.warrant", smooth_k = 25 )
  newjersey$sm2 = smoother( newjersey, t0=t0, "n.warrant", smooth_k = 11 )
  gg = newjersey %>% dplyr::select( month, sm1, sm2, n.warrant ) %>%
    gather( sm1, sm2, n.warrant, key="series", value="n.warrant" )
  gg = filter( gg, month > -20 )
  ggplot( gg, aes( month, n.warrant, col=series ) ) +
    geom_line()
  debug( smoother )
}



######  Fit alternate models  ######

if ( FALSE ) {
  

# Fit unsmoothed seasonality model and make envelope (no temp)
envelope = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                  summarize = TRUE, smooth=FALSE,
                                  fit_model = fit_season_model_q )

head( envelope )

plt <- make_envelope_graph( envelope, t0=t0 ) +
  #geom_line( aes( y=Ystar ) )  +
  geom_vline( xintercept=c(0,t0), col="red", lty=c(2,1) ) +
  geom_hline( yintercept = 0 ) +
  geom_line( aes( y=Ybar ), col="blue" )

plt
ggsave( "my_examples/plots/new_jersey_q.pdf",width=FIG_WIDTH, height=FIG_HEIGHT )



head( newjersey )
envelope.sin = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                  summarize = TRUE, smooth=FALSE,
                                  fit_model = fit_season_model_sin )

plt <- make_envelope_graph( envelope.sin, t0=t0 ) +
  geom_line( aes( y=Ystar ) )
plt
ggsave( "my_examples/plots/new_jersey_sin.pdf",width=FIG_WIDTH, height=FIG_HEIGHT )




envelope.q = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                  summarize = TRUE, smooth=FALSE,
                                  fit_model = fit_season_model_q )
plt <- make_envelope_graph( envelope.q, t0=t0 ) +
  geom_line( aes( y=Ystar ) )
plt + geom_hline(yintercept= 0 )
ggsave( "my_examples/plots/new_jersey_q.pdf", width=FIG_WIDTH, height=FIG_HEIGHT )



}





##### Seeing how autoregression without seasonality modeling works for New Jersey  #####

envelope.sm = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                  summarize = TRUE, smooth=FALSE )

save.nj.plot( envelope.sm, filename = "new_jersey_no_season" )


# See how autocorrelation compares 
M1 = fit_season_model_qtemp( dat = filter( newjersey, month <= t0 ), 
                             "n.warrant", lagless=FALSE )
summary( M1 )
class( M1 )

M0 = fit_model_default( dat = filter( newjersey, month <= t0 ), 
                        "n.warrant", lagless=FALSE )
summary( M0 )
class( M0 )

#stargazer::stargazer( M0, M1, type = "text" )


