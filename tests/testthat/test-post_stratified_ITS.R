test_that("vague tests of post_stratified_ITS", {
 
  R = 10
  data( "meck_subgroup")
  t0 = 0
  meck = meck_subgroup
  head( meck )
  tmax = max( meck$month )
  
  pis = calculate_group_weights( "category", Nname = "n.cases", meck, t0, tmax )
  pis
  expect_equal( nrow(pis), 3 )
  head( meck )
  adjdat = adjust_data( meck, "pbail", "category", Nname = "n.cases", pi_star=pis, include_aggregate=TRUE )
  head( adjdat )
  expect_true( all( c( "pbail_felony", "pbail_misdem", "pbail_traffic" ) %in% colnames(adjdat) ) )
  
  # Modeling adjusted and not
  envelope.adj = process_outcome_model( "pbail.adj", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )
  head( envelope.adj )
  expect_equal( nrow( envelope.adj ), nrow( adjdat ) )
  
  ## And with counts
  
  adjdat = adjust_data( meck, "n.bail", "category", Nname = "n.cases", pis, include_aggregate = TRUE, is_count = TRUE )
  # Modeling adjusted and not
  envelope.adj = process_outcome_model( "n.bail.adj", adjdat, t0=t0, R = 100, summarize = TRUE, smooth=FALSE )
  expect_equal( nrow( envelope.adj ), nrow( adjdat ) )
  
  
  
})


test_that("aggregate_data", {
  
  R = 10
  
  data( "meck_subgroup")
  meck = mutate( meck_subgroup, pbail = 100 * pbail )
  head( meck )
  
  meck = rename( meck, N = n.cases )
  
  ad = aggregate_data( meck, "pbail", "category", Nname = "N" )
  expect_true( is.data.frame(ad ) )
  
  expect_error( aggregate_data( meck, "pbail2", "category", Nname = "N" ) )
  expect_error( aggregate_data( meck, "pbail2", "category", Nname = "N", covariates = "foo" ) )
  
} )



test_that( "generate_fake_grouped_data works", {
  
  fd = generate_fake_grouped_data( -10, 0, 10 )
  head( fd )
  expect_equal( nrow( fd ), 21 * 2 ) 
  
  
  fd2 = generate_fake_grouped_data( -10, 0, 10, method = "jersey" )
  head( fd2 )
  expect_equal( nrow( fd2 ), 21 * 3 ) 
  
} )
