

fm = formula( M0 )
fm
names( fm )
terms( fm )
vrs = all.vars( fm )
lgs = grep( "lag.", vrs, value=TRUE )
lgs
fmup = paste0( "~ . - ", paste( lgs, collapse =" - " ), collapse="" )
fmup

upf = update.formula( fm, formula( fmup ) )
upf

update( M0, fmup )


vrs = all.vars( upf )
vrs
vv = setdiff( vrs, c("Y","month" ) )
vv
lgs = paste0( "lag.", vv, collapse = " + " )
lgs
update.formula( upf, paste0( "~ . + ", lgs ) )

