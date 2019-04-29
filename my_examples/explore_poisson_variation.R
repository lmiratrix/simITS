
# This shows how the month to month variation is far larger than what a poisson model would suggest


data( "meck_subgroup")
meck = mutate( meck_subgroup, pbail = 100 * pbail )
head( meck )
meck = filter( meck, month <= 0 )

m2 = meck %>% group_by( month ) %>%
  summarise( n.bail = sum( n.bail ),
             n.cases = sum( n.cases ),
             pbail = n.bail / n.cases,
             category = "all")
m2
meck = bind_rows( meck, m2 )

sstat = meck %>% group_by( category ) %>% summarise( n = mean( n.cases ),
                                             n.bail = mean( n.bail ),
                                             pbail = n.bail / n )
sstat
sstat = mutate( sstat, n.nobail = n - n.bail )
sstat = as.data.frame( sstat )
rownames( sstat ) = sstat$category
head( meck )

N = nrow( meck )
N
df2 = meck %>% ungroup() %>%  mutate(  
                  n.bail = rpois( N, lambda=sstat[ category, "n.bail" ] ),
                  n.nobail = rpois( N, lambda=sstat[ category, "n.nobail"] ),
                  n = n.bail + n.nobail,
                  pbail = 100 * n.bail / n )

head( df2 )

dd = bind_rows( orig=meck, fake = df2, .id = "source" )
head( dd )
dd$pbail


head( dd ) 

# Remove linear trend to look at resulting variation
calc.resid = function( Y, month ) {
#  browser()
  M = lm( Y ~ month )
  resid( M )
}

dd = dd %>% group_by( source, category ) %>%
  mutate( resid = calc.resid( n.bail, month ) )

dd %>% group_by( source, category ) %>%
  summarise( var.resid = var( resid ),
             sd.resid = sd( resid ),
             sd = sd( n.bail ),
             mn = mean( n.bail ) ) %>%
  arrange( source, mn )

ggplot( dd, aes( month, resid, col = category ) ) +
  facet_wrap( ~ source ) +
  geom_line() + geom_point()


ggplot( dd, aes( month, n.bail, col = category ) ) +
  facet_wrap( ~ source ) +
  geom_line() + geom_point()

