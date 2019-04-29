
# data-raw/process.R

# Process the datafiles used as illustrations in the package

library( tidyverse )
meck = read_csv( "data-raw/mech_month_outcomes.csv")
nrow( meck )
meck = rename( meck, month = month_t )


meck2 = read_csv( "data-raw/CaseLevel190322.csv" )
head( meck2 )
nrow( meck2 )
summary( meck2 )
table( meck2$Severity, useNA= "always" )

meck2.l = meck2 %>% filter( !is.na( Bail ), !is.na( Severity ) ) %>%
  group_by( month, Severity ) %>%
  summarise( n.cases = n(),
             n.bail = sum( Bail ) )

meck2.l = mutate( meck2.l, pbail = n.bail / n.cases )
head( meck2.l )

meck2.l = mutate( meck2.l, 
                  category = fct_recode( Severity,
                                         "felony" = "Felony",
                                         "misdem" = "Misdemeanor",
                                         "traffic" = "Traffic" ) )

meck2.l$Severity = NULL

if ( FALSE ) {
  meck2 = read_csv( "data-raw/PSADataforExample190212.csv")
head( meck2 )


pbails = grep( "pbail.", names( meck2 ) )
ns = 3:5
pbails
ns
times = gsub( "pbail.", "", names( meck2 )[pbails] )
times

meck2.l = reshape( as.data.frame( meck2 ),  idvar=c("month"),
                varying=list( ns, pbails ),
                timevar = "category",
                times = times,
                v.names=c("n","pbail"), direction="long")


meck2.l = mutate( meck2.l, nbail = round( pbail * n.cases ),
                  pbail = pbail * n.cases / n )
head( meck2.l )
}


if ( FALSE ) {
  meck = read_csv( "data-raw/mech_month_outcomes.csv")
  meck = rename( meck, month = month_t )
  head( meck )
  
  meck2 = read_csv( "data-raw/PSADataforExample190212.csv")
  head( meck2 )
  meck2 = mutate( meck2, tot.cases = n.felony + n.misdem + n.traffic )
  summary( meck$tot.cases - meck$n.cases )
  
  meck = merge( meck, meck2, by="month", all=TRUE )
  nrow( meck )
  meck = filter( meck, complete.cases( meck ) )
  head( meck )
  
  
  meck = mutate( meck, 
                 pbail.felony = pbail.felony * n.cases / n.felony,
                 pbail.misdem = pbail.misdem * n.cases / n.misdem,
                 pbail.traffic = pbail.traffic * n.cases / n.traffic )
  
}             

if ( FALSE ) {
  meck.l = gather( meck, pbail, pbail.felony, pbail.misdem, pbail.traffic, key="group", value="pbail" )
  
  meck.n = gather( meck, n.cases, n.felony, n.misdem, n.traffic, key="group", value="n" )
  
  ggplot( meck.l, aes( month, pbail, col=group ) ) +
    geom_line()

  ggplot( meck.n, aes( month, n, col=group ) ) +
    geom_line()
  
  m2 = mutate( meck, ncases2 = n.felony + n.misdem + n.traffic,
                 pbail2 = (n.felony * pbail.felony + n.traffic * pbail.traffic + n.misdem * pbail.misdem) / n.cases )
  
  summary( m2$ncases - m2$ncases2 )
  
  summary( m2$pbail2 - m2$pbail )
  
}


if ( FALSE ) {
  nj = read_csv("data-raw/njbymonth.csv" )
  nj = nj[ -c(2:11)]
  head( nj )
  nj = dplyr::select( nj, -H1, -H2 )
  summary( nj$temperature )
  sd( nj$temperature )
}


# Make a new new jersey
set.seed( 1019 )
library( simITS )
nj = make.fake.data( t.min= -7*12, t.max = 12, t0 = 0, rho = 0.5, sd.omega = 6,
                     coef.temp = 0.30 )
head( nj )
ggplot( data=nj, aes( month, Y ) ) +
  geom_line() +
  geom_line( aes( y = Ystr ), col="green" )
nj$Ystr = NULL


mecklenberg = meck
newjersey = nj
meck_subgroup = meck2.l

usethis::use_data(mecklenberg, newjersey, meck_subgroup, overwrite = TRUE)


if ( FALSE ) {
  sinew::makeOxygen(mecklenberg, add_fields = "source")
  sinew::makeOxygen(newjersey, add_fields = "source")
  sinew::makeOxygen(meck_subgroup, add_fields = "source")
  
}
