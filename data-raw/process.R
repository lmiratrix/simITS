
# data-raw/process.R

# Process the datafiles used as illustrations in the package

library( tidyverse )
meck = read_csv( "data-raw/mech_month_outcomes.csv")

if ( FALSE ) {
  nj = read_csv("data-raw/njbymonth.csv" )
  nj = nj[ -c(2:11)]
  head( nj )
  nj = dplyr::select( nj, -H1, -H2 )
  summary( nj$temperature )
  sd( nj$temperature )
}

meck = rename( meck, month = month_t )

# Make a new new jersey
set.seed( 1019 )
library( simITS )
nj = make.fake.data( t.min= -7*12, t.max = 12, t0 = 0, rho = 0.5, sd.omega = 6 )
head( nj )
ggplot( data=nj, aes( month, Y ) ) +
  geom_line() +
  geom_line( aes( y = Ystr ), col="green" )
nj$Ystr = NULL


mecklenberg = meck
newjersey = nj
usethis::use_data(mecklenberg, newjersey, overwrite = TRUE)


if ( FALSE ) {
  sinew::makeOxygen(mecklenberg, add_fields = "source")
  sinew::makeOxygen(newjersey, add_fields = "source")

}
