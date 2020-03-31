test_that("process_outcome_model works", {

  R = 10
  t0 = -8
  
  data( newjersey )

  season_model =  make_fit_season_model( ~ temperature )

  newjersey = add_lagged_covariates(newjersey, "n.warrant", covariates = season_model )
  head( newjersey )
  expect_true( !is.null( newjersey$lag.temperature ) )
  expect_true( !is.null( newjersey$lag.outcome ) )
  
  mod = season_model( dat = filter( newjersey, month <= t0 ), "n.warrant", lagless=TRUE )
  summary( mod )

  mod = season_model( dat = filter( newjersey, month <= t0 ), "n.warrant", lagless=FALSE )
  summary( mod )
  expect_equal( names( coef( mod ) ),
                c("(Intercept)", "month", "temperature", "lag.outcome", "lag.temperature") )

  # Fit unsmoothed seasonality model and make envelope
  envelope = process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                    summarize = TRUE, smooth=FALSE,
                                    fit_model = season_model )
  head( envelope )
  expect_equal( names( envelope ),
      c("month", "Ymin", "Ymax", "range", "SE", "Ystar", "Y", "Ysmooth", "Ysmooth1", "Ybar" ) )

})


test_that("process_outcome_model extra covariate drop works", {
  
  R = 10
  t0 = -8
  
  data( newjersey )
  head( newjersey )
  season_model =  make_fit_season_model( ~ sin.m + cos.m )
  
  newjersey = add_lagged_covariates(newjersey, "n.warrant", covariates = season_model )
  head( newjersey )
  expect_true( !is.null( newjersey$lag.sin.m ) )
  expect_true( !is.null( newjersey$lag.cos.m ) )
  expect_true( !is.null( newjersey$lag.outcome ) )
  expect_true( sum( newjersey$lag.sin.m == newjersey$sin.m, na.rm=TRUE ) == 0 )
  
  mod = season_model( dat = filter( newjersey, month <= t0 ), "n.warrant", lagless=TRUE )
  summary( mod )
  
  mod = season_model( dat = filter( newjersey, month <= t0 ), "n.warrant", lagless=FALSE )
  summary( mod )
  expect_true( sum( is.na( coef(mod) ) ) == 2 )
  

  # Fit unsmoothed seasonality model and make envelope
  expect_warning( envelope <- process_outcome_model( "n.warrant", newjersey, t0=t0, R = R,
                                    summarize = TRUE, smooth=FALSE,
                                    fit_model = season_model ) )
  head( envelope )
  expect_equal( names( envelope ),
                c("month", "Ymin", "Ymax", "range", "SE", "Ystar", "Y", "Ysmooth", "Ysmooth1", "Ybar" ) )
  

} )






test_that("covariate dropping on the models", {
  
  R = 10
  t0 = -8
  
  data( newjersey )
  head( newjersey )
  season_model =  make_fit_season_model( ~ sin.m + cos.m )
  
  newjersey = add_lagged_covariates(newjersey, "n.warrant", covariates = season_model )
  
  mod = season_model( dat = filter( newjersey, month <= t0 ), "n.warrant", lagless=FALSE )
  summary( mod )
  expect_equal( length( coef( mod  ) ), 7 )
  
  expect_warning( rs <- simITS:::drop_extra_covariates( mod, newjersey ) )
  expect_equal( length( coef( rs ) ), 5 )
  
  
  # And no warnings when no need
  rs2 <- simITS:::drop_extra_covariates( rs, newjersey )
  expect_equal( length( coef( rs2 ) ), 5 )
  
  
})



