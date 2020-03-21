

context("test-summarization-testing")


test_that("The four types of smooth vs summarize calls work", {

  data( "mecklenberg")
  t0 = 0
  preds = process_outcome_model( "pbail", mecklenberg,
                                       t0=t0, R = 10,
                                       summarize = FALSE, smooth=FALSE )
  
  sr =  aggregate_simulation_results( mecklenberg, preds, "pbail" )
  sr
  
  expect_equal( length( sr ), 2 )
  expect_equal( length( sr$t ), 10 )
})


