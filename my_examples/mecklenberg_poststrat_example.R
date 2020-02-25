##
## Mecklenberg example used in the methods paper to illustrate post
## stratificaition
##
## This script generates all plots in the post stratification section.
##
## 


library( tidyverse )
library( simITS )

R = 1000

# Make plots look right to me.
library( ggthemes )
my_t = theme_tufte() + theme( legend.position="bottom",
                              legend.direction="horizontal", legend.key.width=unit(1,"cm"),
                              panel.border = element_blank() )
theme_set( my_t )


data( "meck_subgroup")
meck = mutate( meck_subgroup, pbail = 100 * pbail )
head( meck )

meck = rename( meck, N = n.cases )

t0 = 0
tmax = max( meck$month )

pis = calculate.group.weights( "category", meck, t0, tmax )
pis


##
#### The different subgroups changing over time
##

# Looking at changing shares across time
ggplot( meck, aes( month, N, col=category ) ) +
  geom_point() + geom_line() +
  geom_smooth( se=FALSE, lty=2 )
ggsave( "my_examples/plots/meck_changing_count.pdf", width=5, height=3.75 )


##
## The proportion as outcome
##

adjdat = adjust.data( meck, "pbail", "category", pis, include.aggregate=TRUE )
head( adjdat )

# Looking at changing shares across time
aa = adjdat %>% dplyr::select( month, starts_with( "pi" )) %>%
  gather( pi.felony, pi.misdem, pi.traffic, key="category", value="proportion" )
head( aa )
ggplot( aa, aes( month, proportion, col=category ) ) +
  geom_point() + geom_line()
ggsave( "my_examples/plots/meck_changing_prop.pdf", width=5, height=3.75 )

# Modeling adjusted and not
envelope.adj = process.outcome.model( "pbail.adj", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )

envelope = process.outcome.model( "pbail", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )

head( adjdat )
envelope.felony = process.outcome.model( "pbail.felony", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )
envelope.misdem = process.outcome.model( "pbail.misdem", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )
envelope.traffic = process.outcome.model( "pbail.traffic", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )

env = bind_rows( raw=envelope, adjusted=envelope.adj, felony=envelope.felony, misdem=envelope.misdem, traffic = envelope.traffic, .id="model")
head( env )
plt <- ggplot( env, aes( month, col=model ) ) +
  geom_line( aes(y=Ystar), lty=2 ) +
  geom_line( aes(y=Y)) + geom_point( aes( y=Y ), size=0.5 ) +
  #geom_line( aes(y=Ysmooth1), lty=2 ) +
  geom_vline( xintercept=t0 )

plt

plt +         facet_wrap( ~model )

# Just look at adjusted vs raw
env2 = bind_rows( raw=envelope, adjusted=envelope.adj, .id="model")

plt <- ggplot( env2, aes( month, col=model ) ) +
  geom_line( aes(y=Ystar), lty=2 ) +
  geom_line( aes(y=Y)) + geom_point( aes( y=Y ), size=0.5 ) +
  #geom_line( aes(y=Ysmooth1), lty=2 ) +
  geom_vline( xintercept=t0 )

plt
ggsave( "my_examples/plots/meck_adjusted.pdf", width=5, height=3.5 )



##
## And with counts
##

head( meck )
adjdat = adjust.data( meck, "n.bail", "category", pis, include.aggregate = TRUE, is.count = TRUE )
head( adjdat )


m2 = gather( meck, N, n.bail, key="var", value="count" )
ggplot( m2, aes( month, count, col=category ) ) +
  facet_wrap( ~ var ) +
  geom_line() + geom_point() +
  geom_smooth( data=filter( m2, month <= t0 ), method="lm", fullrange= TRUE, se = FALSE,  )

# Sanity check
#mm = meck %>% group_by( month ) %>%
#  summarise( N = sum( N ),
#             n.bail = sum( n.bail ) )
#plot( mm$n.cases, mm$N )
#summary( mm$n.cases - mm$N )

# Did the counts shift much?
qplot( n.bail, n.bail.adj, data=adjdat )
ggplot( adjdat, aes( x=month) ) +
  geom_line( aes( y=n.bail ) ) +
  geom_line( aes( y=n.bail.adj ), col="green" )


aa = adjdat %>% dplyr::select( month, starts_with("n.bail" ) ) %>%
  gather( starts_with( "n.bail" ), key="category", value="n.bail" )
head( aa )
ggplot( aa, aes( x=month, y=n.bail, col=category ) ) +
  geom_line() + geom_point()

# Modeling adjusted and not
envelope.adj = process.outcome.model( "n.bail.adj", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

envelope = process.outcome.model( "n.bail", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

envelope.misdem = process.outcome.model( "n.bail.misdem", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
envelope.felony = process.outcome.model( "n.bail.felony", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
envelope.traffic = process.outcome.model( "n.bail.traffic", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )

env = bind_rows( raw=envelope, adjusted=envelope.adj, felony=envelope.felony, misdem=envelope.misdem, traffic=envelope.traffic, .id="model")
head( env )

plt <- ggplot( env, aes( month, col=model ) ) +
  geom_line( aes(y=Ystar), lty=2 ) +
  geom_line( aes(y=Y)) + geom_point( aes( y=Y ), size=0.5 ) +
  #geom_line( aes(y=Ysmooth1), lty=2 ) +
  geom_vline( xintercept=t0 )

plt

plt +         facet_wrap( ~model ) +
  geom_line( aes(y=Ybar ), lty=1 )


head( env )
env = mutate( env, impact = Y - Ybar,
              agg = model %in% c("adjusted", "raw"))


plt <- ggplot( env, aes( month,y=impact, col=model, lty=agg ) ) +
  geom_line( lwd=1) + geom_point() +
  geom_vline( xintercept=t0 ) +
  geom_hline( yintercept= 0 )

plt

