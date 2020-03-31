

context("test-summarization-testing")


test_that("Aggregation of simulation results works", {
  
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



test_that("Alternate function works for aggregation", {
  
  agr_2 = function( res, outcomename,
                    months = 1:54,
                    ... ) {
    
    mts = dplyr::filter( res, month %in% months )
    median( mts[[outcomename]] )
  }
  
    
    
  data( "mecklenberg")
  t0 = 0
  preds = process_outcome_model( "pbail", mecklenberg,
                                 t0=t0, R = 10,
                                 summarize = FALSE, smooth=FALSE )
  
  sr =  aggregate_simulation_results( mecklenberg, preds, "pbail", summarizer = agr_2, months=1:5 )
  sr
  
  expect_equal( length( sr ), 2 )
  expect_equal( length( sr$t ), 10 )
})


