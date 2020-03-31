test_that("make_envelope_graph works", {
  
  data( "mecklenberg")
  t0 = 0
  
  # Make the envelope from 10,000 trials (incorporating model uncertainty)
  envelope = process_outcome_model( "pbail", mecklenberg,
                                    t0=0, R = 10,
                                    summarize = TRUE, smooth=FALSE )
  
  # Envelope from plug (too narrow)
  require( ggplot2 )
  grp <- make_envelope_graph(envelope = envelope, t0 = 0) +
    labs( x="month", y="proportion given bail") +
    geom_line( aes(y=Ystar ) )
  expect_true( !is.null( grp ) & is.ggplot(grp) )  
  
  envelope = process_outcome_model( "pbail", mecklenberg,
                                    t0=0, R = 10,
                                    summarize = TRUE, smooth=TRUE )
  
  grp2 <- make_envelope_graph(envelope = envelope, t0 = 0) +
    labs( x="month", y="proportion given bail") +
    geom_line( aes(y=Ystar ) )
  expect_true( !is.null( grp ) & is.ggplot(grp) )    
})
