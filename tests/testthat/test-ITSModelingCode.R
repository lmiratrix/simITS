

test_that("smooth_series call", {
  
  simData <- generate_fake_data()
  head( simData )
  
  Ysmooth1 = smooth_series( simData, outcomename="Y", t0=0 )
  expect_equal( length( Ysmooth1 ), nrow( simData ) )
  
  Ysmooth2 = smooth_series( simData, outcomename="Y", t0=0, post.only = FALSE )
  expect_equal( length( Ysmooth2 ), nrow( simData ) )
  expect_true( all( !is.na( Ysmooth2 ) ) )
  

})






test_that("smooth_residuals call", {
  
  simData <- generate_fake_data()
  head( simData )
  
  fm = make_fit_season_model( ~ Q1 + Q2 + Q3 )
  
  Ysmooth1 = smooth_residuals( simData, outcomename="Y", fit_model = fm,
                               covariates = simData,
                               t0=0 )
  expect_equal( length( Ysmooth1 ), nrow( simData ) )
  expect_false( all( !is.na( Ysmooth1 ) ) )
  
  Ysmooth2 = smooth_residuals( simData, outcomename="Y", fit_model = fm,
                               covariates = simData,
                               t0=0,
                               post.only = FALSE )
  expect_equal( length( Ysmooth2 ), nrow( simData ) )
  expect_true( all( !is.na( Ysmooth2 ) ) )
  
  
  Ysmooth3 = smooth_residuals( simData, outcomename="Y", fit_model = fm,
                               covariates = simData,
                               t0=0,
                               post.only = FALSE, 
                               full_output = TRUE )
  head( Ysmooth3 )
  expect_true( is.data.frame( Ysmooth3 ) )
  expect_equal( nrow( Ysmooth3 ), nrow( simData ) )

})


test_that( "extrapolate_model works", {
  dat <- generate_fake_data()
  dat = add_lagged_covariates( dat, "Y"  )
  dat.pre = dplyr::filter( dat, month <= 0 )
  
  M0 = fit_model_default( dat.pre, "Y" )

  res = extrapolate_model( M0, "Y", dat, 0, 4, summarize=FALSE, smooth=FALSE,
                           fix_parameters = TRUE )
  expect_equal( nrow( res ), 4 * nrow( dat ) )
  
  res = extrapolate_model( M0, "Y", dat, 0, 4, summarize=FALSE, smooth=FALSE,
                           fix_parameters = FALSE )
  expect_equal( nrow( res ), 4 * nrow( dat ) )
  
} )




test_that( "Catches error in missing outcome", {
  
  dat <- generate_fake_data( rho = 2 )
  plot( dat$Y )
  expect_error( dat2 <- add_lagged_covariates( dat, "YY"  ) )

} )



test_that( "Warning in exponential blowup works", {
  
    dat <- generate_fake_data( rho = 2 )
    plot( dat$Y )
    dat = add_lagged_covariates( dat, "Y"  )
    dat.pre = dplyr::filter( dat, month <= 0 )
    
    M0 = fit_model_default( dat.pre, "Y" )
    M0
    
    res = extrapolate_model( M0, "Y", dat, 0, 4, summarize=FALSE, smooth=FALSE,
                             fix_parameters = TRUE )
    expect_equal( nrow( res ), 4 * nrow( dat ) )
    
    
    expect_warning( res2 <- extrapolate_model( M0, "Y", dat, 0, 4, summarize=FALSE, smooth=FALSE,
                             fix_parameters = FALSE ) )
    expect_equal( nrow( res2 ), 4 * nrow( dat ) )
    
} )
  
