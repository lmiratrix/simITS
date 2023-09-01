test_that("smoothing works", {
  
  data( newjersey )
  fit_season_model_qtemp =  make_fit_season_model( ~ temperature + Q2 + Q3 + Q4 )
  
  envelope = process_outcome_model( newjersey, "n.warrant", "month", t0=-7, R = 100, 
                                    summarize = TRUE, smooth=TRUE, 
                                    fit_model = fit_season_model_qtemp )

  expect_true( sum( !is.na( envelope$Ysmooth )) == sum( newjersey$month > -7 ) )
})
