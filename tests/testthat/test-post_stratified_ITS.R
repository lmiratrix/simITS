test_that("vague tests of post_stratified_ITS", {
 
  R = 10
  data( "meck_subgroup")
  meck = rename( meck_subgroup, N = n.cases )
  t0 = 0
  tmax = max( meck$month )
  
  pis = calculate_group_weights( "category", meck, t0, tmax )
  pis
  expect_equal( nrow(pis), 3 )
  
  adjdat = adjust_data( meck, "pbail", "category", pi_star=pis, include_aggregate=TRUE )
  head( adjdat )
  expect_true( all( c( "pbail_felony", "pbail_misdem", "pbail_traffic" ) %in% colnames(adjdat) ) )
  
  # Modeling adjusted and not
  envelope.adj = process_outcome_model( "pbail.adj", adjdat, t0=t0, R = R, summarize = TRUE, smooth=FALSE )
  head( envelope.adj )
  expect_equal( nrow( envelope.adj ), nrow( adjdat ) )
  
  ## And with counts
  
  adjdat = adjust_data( meck, "n.bail", "category", pis, include_aggregate = TRUE, is_count = TRUE )
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
  
  ad = aggregate_data( meck, "pbail", "category" )
  expect_true( is.data.frame(ad ) )
  
  expect_error( aggregate_data( meck, "pbail2", "category" ) )
  expect_error( aggregate_data( meck, "pbail2", "category", covariates = "foo" ) )
  
} )



test_that( "make_fake_group_data works", {
  
  fd = make_fake_group_data( -10, 0, 10 )
  head( fd )
  expect_equal( nrow( fd ), 21 * 2 ) 
  
  
  fd2 = make_fake_group_data( -10, 0, 10, method = "jersey" )
  head( fd2 )
  expect_equal( nrow( fd2 ), 21 * 3 ) 
  
} )
